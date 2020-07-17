package chatapp

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, ActorContext}

import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.Breaks.{breakable, break}

import chatapp.utils.random.{SimpleRand, DiceRoll}

object Action extends Enumeration {
  type Action = Value
  val Post, PostDelivery, Leave, Invite, Compute, Ignore, Error, None = Value
}
import Action._

class BehaviorFactory(compute: Int, post: Int, leave: Int, invite: Int) {
  def apply(dice: DiceRoll): Action = {
    val pick = dice()
    var action: Action = None

    if (pick < compute) {
      action = Compute
    } else if (pick < post) {
      action = Post
    } else if (pick < leave) {
      action = Leave
    } else if (pick < invite) {
      action = Invite
    }

    action
  }
}


sealed trait ChatMsg
final case class ChatPost(payload: Array[Byte], accumulator: ActorRef[Accumulator]) extends ChatMsg
final case class ChatJoin(client: ActorRef[Client], accumulator: ActorRef[Accumulator]) extends ChatMsg
final case class ChatLeave(client: ActorRef[Client], didLogout: Boolean, accumulator: ActorRef[Accumulator]) extends ChatMsg

class Chat(initiator: ActorRef[Client]) {

  val members = new mutable.ArrayBuffer[ActorRef[Client]]()
  var buffer  = new mutable.ArrayBuffer[Array[Byte]]()

  members.append(initiator)

  def apply(): Behavior[ChatMsg] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case ChatPost(payload, accumulator) =>
          post(payload, accumulator)
          Behaviors.same
        case ChatJoin(client, accumulator) =>
          join(client, accumulator)
          Behaviors.same
        case ChatLeave(client, didLogout, accumulator) =>
          leave(client, didLogout, accumulator)
          Behaviors.same
      }
    }

  private def post(payload: Array[Byte], accumulator: ActorRef[Accumulator]) {
    buffer.append(payload)

    if (!members.isEmpty) {
      accumulator ! AccBump(Action.Post, members.length)

      for (member <- members) {
        member ! ClForward(payload, accumulator)
      }
    } else {
      accumulator ! AccStop(Post)
    }
  }

  private def join(client: ActorRef[Client], accumulator: ActorRef[Accumulator]) {
    members.append(client)

    if (!buffer.isEmpty) {
      accumulator ! AccBump(Ignore, buffer.length)

      for (message <- buffer) {
        client ! ClForward(message, accumulator)
      }
    }

    client ! ClAccepted(accumulator)
  }

  private def leave(client: ActorRef[Client], didLogout: Boolean, accumulator: ActorRef[Accumulator]) {
    breakable {
      for ((c, i) <- members.zipWithIndex) {
        if (c == client) {
          members.remove(i)
          break
        }
      }
    }

    client ! ClLeft(didLogout, accumulator)
  }
}

sealed trait ClientMsg
final case class ClBefriend(client: ActorRef[Client]) extends ClientMsg
final case class ClLogout()                           extends ClientMsg
final case class ClLeft(chat: ActorRef[Chat], didLogout: Boolean, accumulator: ActorRef[Accumulator]) extends ClientMsg
final case class ClAccepted(chat: ActorRef[Chat], accumulator: ActorRef[Accumulator]) extends ClientMsg
final case class ClForward(chat: ActorRef[Chat], payload: Array[Byte]) extends ClientMsg
final case class ClAct(behavior: BehaviorFactory, accumulator: ActorRef[Accumulator]) extends ClientMsg


class Client(id: Long, directory: Directory, seed: Long) {

  val friends = mutable.ArrayBuffer[ActorRef[Client]]()
  val chats   = mutable.ArrayBuffer[ActorRef[Chat]]()
  val rand = new SimpleRand(seed)
  val dice = new DiceRoll(rand)
  var fibIndex = 35

  def apply(): Behavior[ClientMsg] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case ClBefriend(client) =>
          befriend(client)
          Behaviors.same
        case ClLogout() =>
          logout()
          Behaviors.same
        case ClLeft(chat, didLogout, accumulator) =>
          left(client, didLogout, accumulator)
          Behaviors.same
        case ClAccepted(chat, accumulator) =>
          accepted(chat, accumulator)
          Behaviors.same
        case ClForward(chat, payload) =>
          forward(chat, payload)
          Behaviors.same
        case ClAct(behavior, accumulator) =>
          act(behavior, accumulator)
          Behaviors.same
      }
    }

  private def befriend(client: ActorRef[Client]) =
    friends.append(client)

  private def logout() {
    if (!chats.isEmpty) {
      for (chat <- chats) {
        chat ! Chat.Leave(true, null)
      }
    } else {
      directory ! Directory.Left()
    }
  }

  private def left(chat: ActorRef[Chat], didLogout: Boolean, accumulator: ActorRef[Accumulator]) {
    for ((c, i) <- chats.zipWithIndex) {
      if (c == chat) {
        chats.remove(i)
      }
    }

    if (chats.isEmpty && didLogout) {
      directory ! Directory.Left()
    } else if (accumulator != null) {
      accumulator ! AccStop(Leave)
    }
  }

  private def accepted(chat: ActorRef[Chat], accumulator: ActorRef[Accumulator]) {
    chats.append(chat)
    accumulator ! AccStop(Ignore)
  }

  private def forward(chat: ActorRef[Chat], payload: Array[Byte]) =
    accumulator ! AccStop(PostDelivery)

  private def act(behavior: BehaviorFactory, accumulator: ActorRef[Accumulator]) {
    val index = rand.nextInt(chats.length)

    behavior(dice) match {
      case Post  => chats(index) ! Chat.Post(null, accumulator)
      case Leave => chats(index) ! Chat.Leave(false, accumulator)
      case Compute => {
        for (i <- 1 to 10000) {
          if (fibonacci(fibIndex) != 9_227_465) {
            accumulator ! AccStop(Error)
            fibIndex += 1
          }
        }

        accumulator ! AccStop(Compute)
      }
      case Invite => {
        val created: ActorRef[Chat] = context.spawn(Chat(context.self))

        chats.append(created)
        rand.shuffle[ActorRef[Client]](friends)

        var invitations = rand.nextInt(friends.length)

        if (invitations == 0) {
          invitations = 1
        }

        for (k <- 0 to invitations - 1) {
          created ! Chat.Join(friends(k), accumulator)
        }

        accumulator ! AccBump(Invite, invitations)
      }
      case None => accumulator ! AccStop(None)
      case default => throw new RuntimeException("This should never happen. Action was " + default)
    }
  }

  private def fibonacci(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1

    val j: Int = n / 2

    val fibJ = fibonacci(j)
    val fibI = fibonacci(j - 1)

    if (n % 2 == 0) {
      return fibJ * (fibJ + (fibI * 2))
    }
    if (n % 4 == 1) {
      return ((fibJ * 2) + fibI) * ((fibJ * 2) - fibI) + 2
    } else {
      return ((fibJ * 2) + fibI) * ((fibJ * 2) - fibI) - 2
    }
  }
}

sealed trait DirMsg
final case class DirLogin(id: Long)           extends DirMsg
final case class DirBefriend()                extends DirMsg
final case class DirLeft(client: ActorRef[Client]) extends DirMsg
final case class DirPoke(factory: BehaviorFactory, accumulator: ActorRef[Accumulator]) extends DirMsg
final case class DirDisconnect(poker: ActorRef[Poker]) extends DirMsg

class Directory(seed: Long, befriend: Int) {
  val clients = mutable.ArrayBuffer[ActorRef[Client]]()
  val random = SimpleRand(seed)
  var poker: ActorRef[Poker] = null

  def apply(): Behavior[DirMsg] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case DirLogin(id) =>
          login(id, context)
          Behaviors.same
        case DirBefriend() =>
          befriend()
          Behaviors.same
        case DirLeft(client) =>
          left(client)
          Behaviors.same
        case DirPoke(factory, accumulator) =>
          poke(chat, accumulator)
          Behaviors.same
        case DirDisconnect(poker) =>
          forward(chat, payload)
          Behaviors.same
      }
    }

  private def login(id: Long, context: ActorContext[DirMsg]) {
    val client = new Client(id, context.self(), random.next())
    clients.append(context.spawn(client(), "Client"))
  }
}

sealed trait AccMsg
final case class AccBump(action: Action, expected: Int)           extends AccMsg
final case class AccStop(action: Action = Ignore)                 extends AccMsg
final case class AccPrint(poker: ActorRef[Poker], i: Int, j: Int) extends AccMsg

class Accumulator(poker: Poker, expected: Long) {

  def apply(): Behavior[AccMsg] =
    Behaviors.unhandled

}

class Poker(parsable: Boolean, clients: Long, turns: Long, directories: Long, befriend: Int, factory: BehaviorFactory) {

}

class Config(args: Array[String]) {
  var clients  = 1024
  var numDirs  = 8
  var turns    = 32
  var compute  = 55
  var post     = 25
  var leave    = 10
  var invite   = 10
  var befriend = 10

  var parsable = false

  readConfig()

  private def readConfig() {
    var i = 0
    while (i < args.length) {
      args(i) match {
        case "-c" => { clients  = args(i + 1).toInt; i += 1 }
        case "-d" => { numDirs  = args(i + 1).toInt; i += 1 }
        case "-t" => { turns    = args(i + 1).toInt; i += 1 }
        case "-m" => { compute  = args(i + 1).toInt; i += 1 }
        case "-p" => { post     = args(i + 1).toInt; i += 1 }
        case "-l" => { leave    = args(i + 1).toInt; i += 1 }
        case "-i" => { invite   = args(i + 1).toInt; i += 1 }
        case "-b" => { befriend = args(i + 1).toInt; i += 1 }
        case "-parse" => { parsable = true }
      }
      i += 1
    }

    if (numDirs > clients) {
      println(s"Invalid arguments! Cannot have more directories ($numDirs) than clients ($clients)")
      System.exit(1)
    }

    if (clients < (numDirs * 2)) {
      println(s"Invalid arguments! Need to have 2x as many dictionaries ($numDirs) so that clients ($clients) have at least 1 friend")
      System.exit(1)
    }

    val sum = compute + post + leave + invite
    if (sum != 100) {
      println("Invalid arguments! Sum of probabilities != 100.")
      println(s"\tIt is: $sum")
      System.exit(1)
    }
  }

  def print {
    println(s"Configuration ChatApp Benchmark")
    println(s"\tclients:     $clients")
    println(s"\tdirectories: $numDirs")
    println(s"\tturns:       $turns")
    println(s"\tcompute:     $compute")
    println(s"\tpost:        $post")
    println(s"\tleave:       $leave")
    println(s"\tinvite:      $invite")
    println(s"\tbefriend:    $befriend")
    println(s"\tparsable:    $parsable")
  }
}

class Runner(configuration: Config) {
  var benchmark: ChatApp
  val result = new Result(configuration.parsable)

  var iterations = 0

  var running = false
  var summarize = false
  var startTime = 0L
  var endTime = 0L

  var completion: Promise[Int]

  sealed trait RunnerMsg
  final case class Start(iter: Int) extends RunnerMsg
  final case class Complete(poker: Poker, last: Boolean) extends RunnerMsg

  def apply(): Behavior[RunnerMsg] =
    Behaviors.setup { context =>
      Behaviors.receiveMessage {
        case Start(iter) =>
          start(iter)
          Behaviors.same

        case Complete(poker, last) =>
          complete(poker, last)
          Behaviors.same
      }
    }

  private def start(iter: Int) {
    // val promise = Promise[Int]()
    // | pp |
    //   pp:: actors createPromisePair.
    //   completionR:: pp resolver.

    //   benchmark:: ChatApp new: configuration.
    //   iterations:: iter.
    //   next: nil isLast: false.
    //   ^ pp promise
    // return promise
  }

  private def complete(poker: Poker, last: Boolean) {
    endTime = System.nanoTime() / 1000L
    running = false

    result.record((endTime - startTime) / 1000.0)
    // context.log.info("Total: {}us", (endTime - startTime) / 1000.0)
    next(poker, last)
  }

  private def next(poker: Poker, last: Boolean) {
    if (running) {
      return
    }

    if (summarize) {
      // Runner.summarize' println
      result.summarize(poker, last)
      summarize = false
    }

    if (iterations > 0) {
      running = true
      startTime = System.nanoTime()

      // context.log.info("Runner.run: {}", iterations)

      benchmark.run(this, iterations == 1)
      iterations -= - 1
      summarize = iterations == 0
    } else {
      completion.complete(0)
    }
  }
  /*



  */
}

object Main extends App {
  val cfg = new Config(args)
  // val runner: ActorSystem[Runner.Start] = ActorSystem(Runner(), "Runner")
  // runner ! Start(100)
}

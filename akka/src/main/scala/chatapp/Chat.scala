package chatapp

import akka.actor.typed.scaladsl.{ActorContext, Behaviors, LoggerOps}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import com.typesafe.config.ConfigFactory

import scala.collection.mutable
import scala.concurrent.Promise
import scala.util.control.Breaks.{break, breakable}
import chatapp.utils.random._

import scala.jdk.CollectionConverters._

object Action extends Enumeration {
  type Action = Value
  val Post, PostDelivery, Leave, Invite, Compute, Ignore, Error, None = Value
}
import Action._

class BehaviorFactory(compute: Int, post: Int, leave: Int, invite: Int) {
  val _compute: Int = compute
  val _post: Int    = _compute + post
  val _leave: Int   = _post + leave
  val _invite: Int  = _leave + invite

  def apply(dice: DiceRoll): Action = {
    val pick: Int      = dice()
    var action: Action = None

    if (pick < _compute) {
      action = Compute
    } else if (pick < _post) {
      action = Post
    } else if (pick < _leave) {
      action = Leave
    } else if (pick < _invite) {
      action = Invite
    }
    action
  }
}

sealed trait ChatMsg
final case class ChatPost(payload: Array[Byte], accumulator: ActorRef[AccMsg])
    extends ChatMsg
final case class ChatJoin(
    client: ActorRef[ClientMsg],
    accumulator: ActorRef[AccMsg]
) extends ChatMsg
final case class ChatLeave(
    client: ActorRef[ClientMsg],
    didLogout: Boolean,
    accumulator: ActorRef[AccMsg]
) extends ChatMsg

class Chat(initiator: ActorRef[ClientMsg]) {

  val members = new mutable.ArrayBuffer[ActorRef[ClientMsg]]()
  var buffer  = new mutable.ArrayBuffer[Array[Byte]]()

  members.append(initiator)

  def apply(): Behavior[ChatMsg] =
    Behaviors.receive { (context, message) =>
      message match {
        case ChatPost(payload, accumulator) =>
          post(context.self, payload, accumulator)
          Behaviors.same
        case ChatJoin(client, accumulator) =>
          join(context.self, client, accumulator)
          Behaviors.same
        case ChatLeave(client, didLogout, accumulator) =>
          leave(context.self, client, didLogout, accumulator)
          Behaviors.same
      }
    }

  private def post(
      chat: ActorRef[ChatMsg],
      payload: Array[Byte],
      accumulator: ActorRef[AccMsg]
  ): Unit = {
    buffer.append(payload)
    if (members.nonEmpty) {
      accumulator ! AccBump(Action.Post, members.length)
      for (member <- members) {
        member ! ClForward(chat, payload, accumulator)
      }
    } else {
      accumulator ! AccStop(Post)
    }
  }

  private def join(
      chat: ActorRef[ChatMsg],
      client: ActorRef[ClientMsg],
      accumulator: ActorRef[AccMsg]
  ): Unit = {
    members.append(client)
    if (buffer.nonEmpty) {
      accumulator ! AccBump(Ignore, buffer.length)
      for (message <- buffer) {
        client ! ClForward(chat, message, accumulator)
      }
    }
    client ! ClAccepted(chat, accumulator)
  }

  private def leave(
      chat: ActorRef[ChatMsg],
      client: ActorRef[ClientMsg],
      didLogout: Boolean,
      accumulator: ActorRef[AccMsg]
  ): Unit = {
    breakable {
      for ((c, i) <- members.zipWithIndex) {
        if (c == client) {
          members.remove(i)
          break
        }
      }
    }
    client ! ClLeft(chat, didLogout, accumulator)
  }
}

sealed trait ClientMsg
final case class ClBefriend(client: ActorRef[ClientMsg]) extends ClientMsg
final case class ClLogout()                              extends ClientMsg
final case class ClLeft(
    chat: ActorRef[ChatMsg],
    didLogout: Boolean,
    accumulator: ActorRef[AccMsg]
) extends ClientMsg
final case class ClAccepted(
    chat: ActorRef[ChatMsg],
    accumulator: ActorRef[AccMsg]
) extends ClientMsg
final case class ClForward(
    chat: ActorRef[ChatMsg],
    payload: Array[Byte],
    accumulator: ActorRef[AccMsg]
) extends ClientMsg
final case class ClAct(behavior: BehaviorFactory, accumulator: ActorRef[AccMsg])
    extends ClientMsg

class Client(id: Long, directory: ActorRef[DirMsg], seed: Long) {

  val friends  = new mutable.ArrayBuffer[ActorRef[ClientMsg]]()
  val chats    = new mutable.ArrayBuffer[ActorRef[ChatMsg]]()
  val rand     = new SimpleRand(seed)
  val dice     = new DiceRoll(rand)
  var fibIndex = 35

  def apply(): Behavior[ClientMsg] = active()

  private def active(): Behavior[ClientMsg] = {
    Behaviors.receive { (context, message) =>
      message match {
        case ClBefriend(client) =>
          befriend(client)
          Behaviors.same
        case ClLogout() =>
          logout()
        case ClLeft(chat, didLogout, accumulator) =>
          left(chat, didLogout, accumulator)
        case ClAccepted(chat, accumulator) =>
          accepted(chat, accumulator)
          Behaviors.same
        case ClForward(chat, payload, accumulator) =>
          forward(chat, payload, accumulator)
          Behaviors.same
        case ClAct(behavior, accumulator) =>
          act(behavior, accumulator)
      }
    }
  }

  private def befriend(client: ActorRef[ClientMsg]) =
    friends.append(client)

  private def logout(): Behavior[ClientMsg] = {
    Behaviors.setup { context =>
      if (chats.nonEmpty) {
        for (chat <- chats) {
          chat ! ChatLeave(context.self, didLogout = true, null)
        }
        active()
      } else {
        directory ! DirLeft(context.self)
        Behaviors.stopped
      }
    }
  }
  private def left(
      chat: ActorRef[ChatMsg],
      didLogout: Boolean,
      accumulator: ActorRef[AccMsg]
  ): Behavior[ClientMsg] = {
    Behaviors.setup { context =>
      for ((c, i) <- chats.zipWithIndex) {
        if (c == chat) {
          chats.remove(i)
        }
      }
      if (chats.isEmpty && didLogout) {
        directory ! DirLeft(context.self)
        Behaviors.stopped
      } else if (accumulator != null) {
        accumulator ! AccStop(Leave)
        active()
      } else {
        active()
      }
    }
  }

  private def accepted(
      chat: ActorRef[ChatMsg],
      accumulator: ActorRef[AccMsg]
  ): Unit = {
    chats.append(chat)
    accumulator ! AccStop(Ignore)
  }

  private def forward(
      chat: ActorRef[ChatMsg],
      payload: Array[Byte],
      accumulator: ActorRef[AccMsg]
  ): Unit =
    accumulator ! AccStop(PostDelivery)

  private def act(
      behavior: BehaviorFactory,
      accumulator: ActorRef[AccMsg]
  ): Behavior[ClientMsg] = {
    Behaviors.setup { context =>
      val index = rand.nextInt(chats.length)

      behavior(dice) match {
        case Post => chats(index) ! ChatPost(null, accumulator)
        case Leave =>
          chats(index) ! ChatLeave(context.self, didLogout = false, accumulator)
        case Compute =>
          for (i <- 1 to 10000) {
            if (fibonacci(fibIndex) != 9_227_465) {
              accumulator ! AccStop(Error)
              fibIndex += 1
            }
          }
          accumulator ! AccStop(Compute)
        case Invite =>
          val chat                       = new Chat(context.self)
          val created: ActorRef[ChatMsg] = context.spawn(chat(), "Chat")
          chats.append(created)
          rand.shuffle[ActorRef[ClientMsg]](friends)
          var invitations = rand.nextInt(friends.length)
          if (invitations == 0) {
            invitations = 1
          }
          accumulator ! AccBump(Invite, invitations)
          for (k <- 0 until invitations) {
            created ! ChatJoin(friends(k), accumulator)
          }
        case None => accumulator ! AccStop(None)
        case default =>
          throw new RuntimeException(
            "This should never happen. Action was " + default
          )
      }
      active()
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
final case class DirLogin(id: Long)                   extends DirMsg
final case class DirBefriend()                        extends DirMsg
final case class DirLeft(client: ActorRef[ClientMsg]) extends DirMsg
final case class DirPoke(
    factory: BehaviorFactory,
    accumulator: ActorRef[AccMsg]
)                                                         extends DirMsg
final case class DirDisconnect(poker: ActorRef[PokerMsg]) extends DirMsg

class Directory(seed: Long, befriend: Int) {
  val clients                    = new mutable.ArrayBuffer[ActorRef[ClientMsg]]()
  val random                     = new SimpleRand(seed)
  var _poker: ActorRef[PokerMsg] = _

  def apply(): Behavior[DirMsg] = active()

  private def active(): Behavior[DirMsg] = {
    Behaviors.receive { (context, message) =>
      message match {
        case DirLogin(id) =>
          login(id)
        case DirBefriend() =>
          befriend()
          Behaviors.same
        case DirLeft(client) =>
          left(client)
        case DirPoke(factory, accumulator) =>
          poke(factory, accumulator)
          Behaviors.same
        case DirDisconnect(poker) =>
          disconnect(poker)
          Behaviors.same
      }
    }
  }

  private def login(id: Long): Behavior[DirMsg] = {
    Behaviors.setup { context =>
      val client = new Client(id, context.self, random.next())
      clients.append(context.spawn(client(), s"Client${id}"))
      active()
    }
  }

  private def befriend(): Unit = {
    for (fclients <- clients) {
      var foundFriend = false
      while (foundFriend) {
        for (client <- clients) {
          if (random.nextInt(100) < befriend && fclients != clients) {
            client ! ClBefriend(fclients)
            fclients ! ClBefriend(client)
            foundFriend = true
          }
        }
      }
    }
  }

  private def left(client: ActorRef[ClientMsg]): Behavior[DirMsg] = {
    Behaviors.setup { context =>
      for ((c, i) <- clients.zipWithIndex) {
        if (c == client) {
          clients.remove(i)
        }
      }
      if (clients.isEmpty) {
        _poker ! PokFinished
        Behaviors.stopped
      } else {
        active()
      }
    }
  }

  private def poke(
      factory: BehaviorFactory,
      accumulator: ActorRef[AccMsg]
  ): Unit = {
    for (client <- clients) {
      client ! ClAct(factory, accumulator)
    }
  }

  private def disconnect(poker: ActorRef[PokerMsg]): Unit = {
    _poker = poker
    for (client <- clients) {
      client ! ClLogout()
    }
  }
}

sealed trait AccMsg
final case class AccBump(action: Action, increase: Int) extends AccMsg
final case class AccStop(action: Action = Ignore)       extends AccMsg
final case class AccPrint(collector: ActorRef[PokerMsg], i: Int, j: Int)
    extends AccMsg

class Accumulator(poker: ActorRef[PokerMsg], expected: Long) {
  val startTime: Long   = System.currentTimeMillis()
  var endTime: Long     = 0
  var duration: Double  = 0
  var did_stop: Boolean = false
  var actions: mutable.HashMap[Action.Value, Int] =
    new mutable.HashMap[Action.Value, Int]()
  var _expected: Long = expected

  def apply(): Behavior[AccMsg] = {
    Behaviors.receive { (context, message) =>
      message match {
        case AccBump(action, increase) =>
          count(action)
          _expected = (_expected + increase) - 1
          Behaviors.same
        case AccStop(action) =>
          stop(action)
          Behaviors.same
        case AccPrint(collector, i, j) =>
          collector ! PokCollect(i, j, duration, actions)
          Behaviors.same
      }
    }
  }

  private def count(action: Action): Unit = actions.apply(action) += 1

  private def stop(action: Action): Unit = {
    count(action)
    _expected -= 1
    if (_expected == 0) {
      endTime = System.currentTimeMillis()
      duration = endTime - startTime
      did_stop = true
      poker ! PokConfirm
    }
  }

}

sealed trait PokerMsg
final case object PokFinished extends PokerMsg
final case object PokConfirm  extends PokerMsg
final case class PokCollect(
    i: Int,
    j: Int,
    duration: Double,
    actions: mutable.HashMap[Action, Int]
) extends PokerMsg

class Poker(
    parsable: Boolean,
    clients: Long,
    turns: Long,
    directories: Long,
    befriend: Int,
    factory: BehaviorFactory
) {
  var actions            = new mutable.HashMap[Action, Int]()
  var logouts            = 0
  var confirmations      = 0
  var iterations         = 0
  var directories        = new mutable.ArrayBuffer[ActorRef[DirMsg]]()
  var runtimes           = new mutable.ArrayBuffer[ActorRef[AccMsg]]()
  var accumulations      = 0
  var bench: ActorRef[_] = _
  var last               = false
  var turnSeries         = new mutable.ArrayBuffer[Double]()

  def apply(): Behavior[PokerMsg] = {}
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

  private def readConfig(): Unit = {
    var i = 0
    while (i < args.length) {
      args(i) match {
        case "-c"     => { clients = args(i + 1).toInt; i += 1 }
        case "-d"     => { numDirs = args(i + 1).toInt; i += 1 }
        case "-t"     => { turns = args(i + 1).toInt; i += 1 }
        case "-m"     => { compute = args(i + 1).toInt; i += 1 }
        case "-p"     => { post = args(i + 1).toInt; i += 1 }
        case "-l"     => { leave = args(i + 1).toInt; i += 1 }
        case "-i"     => { invite = args(i + 1).toInt; i += 1 }
        case "-b"     => { befriend = args(i + 1).toInt; i += 1 }
        case "-parse" => { parsable = true }
      }
      i += 1
    }

    if (numDirs > clients) {
      println(
        s"Invalid arguments! Cannot have more directories ($numDirs) than clients ($clients)"
      )
      System.exit(1)
    }

    if (clients < (numDirs * 2)) {
      println(
        s"Invalid arguments! Need to have 2x as many dictionaries ($numDirs) so that clients ($clients) have at least 1 friend"
      )
      System.exit(1)
    }

    val sum = compute + post + leave + invite
    if (sum != 100) {
      println("Invalid arguments! Sum of probabilities != 100.")
      println(s"\tIt is: $sum")
      System.exit(1)
    }
  }

  def print(): Unit = {
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

  var running   = false
  var summarize = false
  var startTime = 0L
  var endTime   = 0L

  var completion: Promise[Int]

  sealed trait RunnerMsg
  final case class Start(iter: Int)                      extends RunnerMsg
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

  private def start(iter: Int): Unit = {
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

  private def complete(poker: Poker, last: Boolean): Unit = {
    endTime = System.nanoTime() / 1000L
    running = false

    result.record((endTime - startTime) / 1000.0)
    // context.log.info("Total: {}us", (endTime - startTime) / 1000.0)
    next(poker, last)
  }

  private def next(poker: Poker, last: Boolean): Unit = {
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
      iterations -= -1
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

use "cli"
use "collections"
use "time"
use "random"
use "util"
use "math"
use "format"
use "term"

type ClientMap is Map[U64, Client]
type FriendSet is SetIs[Client]
type ChatSet is SetIs[Chat]
type ClientSet is SetIs[Client]

primitive Post
primitive Leave
primitive Invite
primitive Compute

type Action is
  ( Post
  | Leave
  | Invite
  | Compute
  | None
  )

type ActionMap is MapIs[Action, U64]

class val BehaviorFactory
  let _compute: U64
  let _post: U64
  let _leave: U64
  let _invite: U64

  new create(compute: U64, post: U64, leave: U64, invite: U64) =>
    _compute = compute
    _post = post
    _leave = leave
    _invite = invite

  fun box apply(dice: DiceRoll): (Action | None) =>
    var action: (Action | None) = None

    if dice(_compute) then
      action = Compute
    elseif dice(_post) then
      action = Post
    elseif dice(_leave) then
      action = Leave
    elseif dice(_invite) then
      action = Invite
    end

    action

actor Chat
  let _members: ClientSet
  var _buffer: Array[(Array[U8] val | None)]

  new create(initiator: Client) =>
    _members = ClientSet
    _buffer =  Array[(Array[U8] val | None)]

    _members.set(initiator)

  be post(payload: (Array[U8] val | None), accumulator: Accumulator) =>
    ifdef "_BENCH_NO_BUFFERED_CHATS" then
      None
    else
      _buffer.push(payload)
    end

    if _members.size() > 0 then
      accumulator.bump(_members.size())

      for member in _members.values() do
        member.forward(this, payload, accumulator)
      end
    else
      accumulator.stop(Post)
    end

  be join(client: Client, accumulator: Accumulator) =>
    _members.set(client)

    ifdef "_BENCH_NO_BUFFERED_CHATS" then
       accumulator.stop(Invite)
    else
      if _buffer.size() > 0 then
        accumulator.bump(_buffer.size())

        for message in _buffer.values() do
          client.forward(this, message, accumulator)
        end
      else
        accumulator.stop(Invite)
      end
    end

  be leave(client: Client, did_logout: Bool, accumulator: (Accumulator | None)) =>
    _members.unset(client)
    client.left(this, did_logout, accumulator)

actor Client
  let _id: U64
  let _friends: FriendSet
  let _chats: ChatSet
  let _directory: Directory
  let _dice: DiceRoll
  let _rand: SimpleRand

  new create(id: U64, directory: Directory, seed: U64) =>
    _id = id
    _friends = FriendSet
    _chats = ChatSet
    _directory = directory
    _rand = SimpleRand(seed)
    _dice = DiceRoll(_rand)


  be befriend(client: Client) =>
    _friends.set(client)

  be logout() =>
    for chat in _chats.values() do
      chat.leave(this, true, None)
    else
      _directory.left(_id)
    end

  be left(chat: Chat, did_logout: Bool, accumulator: (Accumulator | None)) =>
    _chats.unset(chat)

    if ( _chats.size() == 0 ) and did_logout then
      _directory.left(_id)
    else
      match accumulator
      | let accumulator': Accumulator => accumulator'.stop(Leave)
      end
    end

  be invite(chat: Chat, accumulator: Accumulator) =>
    _chats.set(chat)
    chat.join(this, accumulator)

  be forward(chat: Chat, payload: (Array[U8] val | None), accumulator: Accumulator) =>
    accumulator.stop(Post)

  be act(behavior: BehaviorFactory, accumulator: Accumulator) =>
    let index = _rand.nextInt(_chats.size().u32()).usize()
    var i: USize = 0

    // Pony has no implicit conversion from Seq to Array.
    var chat = Chat(this)

    for c in _chats.values() do
      if i == index then
        break
      end

      i = i + 1 ; chat = c
    end

    match behavior(_dice)
    | Post => chat.post(None, accumulator)
    | Leave => chat.leave(this, false, accumulator)
    | Compute => Fibonacci(35) ; accumulator.stop(Compute)
    | Invite =>
      let created = Chat(this)

      // Again convert the set values to an array, in order
      // to be able to use shuffle from rand
      let f = Array[Client](_friends.size())

      for friend in _friends.values() do
        f.push(friend)
      end

      let s = Rand(_rand.next())
      s.shuffle[Client](f)

      f.unshift(this)

      var invitations: USize = s.next().usize() % _friends.size()

      if invitations == 0 then
        accumulator.stop(Invite)
      else
        accumulator.bump(invitations)

        for k in Range[USize](0, invitations) do
          //pick random index k??
          try f(k)?.invite(created, accumulator) end
        end
      end
    else
      accumulator.stop()
    end

actor Directory
  let _clients: ClientMap
  let _random: SimpleRand
  let _befriend: U64
  var _poker: (Poker | None)

  new create(seed: U64, befriend: U64) =>
    _clients = ClientMap
    _random = SimpleRand(seed)
    _befriend = befriend
    _poker = None

  be login(id: U64) =>
    let new_client = Client(id, this, _random.next())
    let befriend: U32 = _befriend.u32()

    _clients(id) = new_client

    for client in _clients.values() do
      if _random.nextInt(100) < befriend then
        client.befriend(new_client)
        new_client.befriend(client)
      end
    end

  be logout(id: U64) =>
    try
      _clients(id)?.logout()
    end

  be left(id: U64) =>
    try
      _clients.remove(id)?

      if _clients.size() == 0 then
        match _poker
        | let poker: Poker => poker.finished()
        end
      end
    end

  be poke(factory: BehaviorFactory, accumulator: Accumulator) =>
    for client in _clients.values() do
      client.act(factory, accumulator)
    end

  be disconnect(poker: Poker) =>
    _poker = poker

    for c in _clients.values() do
      c.logout()
    end

actor Accumulator
  let _poker: Poker
  var _actions: ActionMap iso
  var _start: F64
  var _end: F64
  var _duration: F64
  var _expected: USize
  var _did_stop: Bool

  new create(poker: Poker, expected: USize) =>
    _poker = poker
    _actions = recover ActionMap end
    _start = Time.millis().f64()
    _end = 0
    _duration = 0
    _expected = expected
    _did_stop = false

  be bump(expected: USize) =>
    _expected = ( _expected + expected ) - 1

  be stop(action: Action = None) =>
    try
      _actions(action) = _actions(action)? + 1
    else
      _actions(action) = 1
    end

    if (_expected = _expected - 1) == 1 then
      _end = Time.millis().f64()
      _duration = _end - _start
      _did_stop = true

      _poker.confirm()
    end

   be print(poker: Poker, i: USize, j: USize) =>
     poker.collect(i, j, _duration, _actions = recover ActionMap end)

actor Poker
  let _actions: ActionMap
  var _clients: U64
  var _logouts: USize
  var _confirmations: USize
  var _turns: U64
  var _iteration: USize
  var _directories: Array[Directory] val
  var _runtimes: Array[Accumulator]
  var _accumulations: USize
  var _finals: Array[Array[F64]]
  var _factory: BehaviorFactory
  var _bench: (AsyncBenchmarkCompletion | None)
  var _last: Bool
  var _turn_series: Array[F64]

  new create(clients: U64, turns: U64, directories: USize, befriend: U64, factory: BehaviorFactory) =>
    _actions = ActionMap
    _clients = clients
    _logouts = 0
    _confirmations = 0
    _turns = turns
    _iteration = 0
    _runtimes = Array[Accumulator]
    _accumulations = 0
    _finals = Array[Array[F64]]
    _factory = factory
    _bench = None
    _last = false
    _turn_series = Array[F64]

    let rand = SimpleRand(42)

    _directories = recover
      let dirs = Array[Directory](directories)

      for i in Range[USize](0, directories.usize()) do
        dirs.push(Directory(rand.next(), befriend))
      end

      dirs
    end


  be apply(bench: AsyncBenchmarkCompletion, last: Bool) =>
    _confirmations = _turns.usize()
    _logouts = _directories.size()
    _bench = bench
    _last = last
    _accumulations = 0

    var turns: U64 = _turns
    var index: USize = 0
    var values: Array[F64] = Array[F64].init(0, _turns.usize())

    _finals.push(values)

    for client in Range[U64](0, _clients) do
      try
        index = client.usize() % _directories.size()
        _directories(index)?.login(client)
      end
    end

    while ( turns = turns - 1 ) >= 1 do
      let accumulator = Accumulator(this, _clients.usize())

      for directory in _directories.values() do
        directory.poke(_factory, accumulator)
      end

      _runtimes.push(accumulator)
    end

  be confirm() =>
    if (_confirmations = _confirmations - 1 ) == 1 then
      for d in _directories.values() do
        d.disconnect(this)
      end
    end

  be finished() =>
    if (_logouts = _logouts - 1 ) == 1 then
      var turn: USize = 0

      for accumulator in _runtimes.values() do
        _accumulations = _accumulations + 1
        accumulator.print(this, _iteration, turn)
        turn = turn + 1
      end

      _runtimes = Array[Accumulator]
    end

  be collect(i: USize, j: USize, duration: F64, actions: ActionMap val) =>
    for (key, value) in actions.pairs() do
      try
        _actions(key) = value + _actions(key)?
      else
        _actions(key) = value
      end
    end

    try
      _finals(i)?(j)? = duration
      _turn_series.push(duration)
    end

    if ( _accumulations = _accumulations - 1 ) == 1 then
      _iteration = _iteration + 1

      match _bench
      | let bench: AsyncBenchmarkCompletion => bench.complete()

        if _last then
          let stats = SampleStats(_turn_series = Array[F64])
          var turns = Array[Array[F64]]
          var qos = Array[F64]

          for k in Range[USize](0, _turns.usize()) do
            try
              turns(k)?
            else
              turns.push(Array[F64])
            end

            for iter in _finals.values() do
              try turns(k)?.push(iter(k)?) end
            end
          end

          for l in Range[USize](0, turns.size()) do
            try qos.push(SampleStats(turns.pop()?).stddev()) end
          end

          bench.append(
            "".join(
              [ ANSI.bold()
                Format("" where width = 31)
                Format("j-mean" where width = 18, align = AlignRight)
                Format("j-median" where width = 18, align = AlignRight)
                Format("j-error" where width = 18, align = AlignRight)
                Format("j-stddev" where width = 18, align = AlignRight)
                Format("quality of service" where width = 32, align = AlignRight)
                ANSI.reset()
              ].values()
            )
          )

          bench.append(
            "".join([
                Format("Turns" where width = 31)
                Format(stats.mean().string() + " ms" where width = 18, align = AlignRight)
                Format(stats.median().string() + " ms" where width = 18, align = AlignRight)
                Format("±" + stats.err().string() + " %" where width = 18, align = AlignRight)
                Format(stats.stddev().string() where width = 18, align = AlignRight)
                Format(SampleStats(qos = Array[F64]).median().string() where width = 32, align = AlignRight)
              ].values()
            )
          )

          bench.append("")

          for (key, value) in _actions.pairs() do
            // could make 'Actions' stringable
            let identifier =
              match key
              | Post    => "Post"
              | Leave   => "Leave"
              | Invite  => "Invite"
              | Compute => "Compute"
              | None    => "None"
              end

            bench.append(
              "".join([
                  Format(identifier where width = 8)
                  Format(value.string() where width = 10, align = AlignRight)
                ].values()
              )
            )
          end
        end
      end
    end

class iso ChatApp is AsyncActorBenchmark
  var _clients: U64
  var _turns: U64
  var _factory: BehaviorFactory val
  var _poker: Poker

  new iso create(env: Env, cmd: Command val) =>
    _clients = cmd.option("clients").u64()
    _turns = cmd.option("turns").u64()

    let directories: USize = cmd.option("directories").u64().usize()
    let compute: U64 = cmd.option("compute").u64()
    let post: U64 = cmd.option("post").u64()
    let leave: U64 = cmd.option("leave").u64()
    let invite: U64 = cmd.option("invite").u64()
    let befriend: U64 = cmd.option("befriend").u64()

    _factory = recover BehaviorFactory(compute, post, leave, invite) end

    _poker = Poker(_clients, _turns, directories, befriend, _factory)

  fun box apply(c: AsyncBenchmarkCompletion, last: Bool) => _poker(c, last)

  fun tag name(): String => "Chat App"

  actor Main is BenchmarkRunner
    new create(env: Env) =>
      try
        let cs =
          recover
            CommandSpec.leaf("chat-app", "Cross Language Actor Benchmark", [
              OptionSpec.u64("clients", "The number of clients. Defaults to 1024."
                where short' = 'c', default' = U64(1024))
              OptionSpec.u64("directories", "The number of directories. Defaults to 8."
                where short' = 'd', default' = U64(8))
              OptionSpec.u64("turns", "The number of turns. Defaults to 32."
                where short' = 't', default' = U64(32))
              OptionSpec.u64("compute", "The compute behavior probability. Defaults to 75."
                where short' = 'm', default' = U64(75))
              OptionSpec.u64("post", "The post behavior probability. Defaults to 25."
                where short' = 'p', default' = U64(25))
              OptionSpec.u64("leave", "The leave behavior probability. Defaults to 25."
                where short' = 'l', default' = U64(25))
              OptionSpec.u64("invite", "The invite behavior probability. Defaults to 25."
                where short' = 'i', default' = U64(25))
              OptionSpec.u64("befriend", "The befriend probability. Defaults to 10."
                where short' = 'b', default' = U64(10))
              OptionSpec.bool("parseable", "Generate parseable output. Defaults to false."
                where short' = 's', default' = false)
            ])? .> add_help()?
          end

        let result = recover val CommandParser(consume cs).parse(env.args, env.vars) end

        match result
        | let cmd: Command val => Runner(env, this, cmd)
        | let help: CommandHelp val => help.print_help(env.out) ; env.exitcode(0)
        | let err: SyntaxError val => env.out.print(err.string()) ; env.exitcode(1)
        end
      else
        env.exitcode(-1)
        return
      end

  fun tag benchmarks(bench: Runner, env: Env, cmd: Command val) =>
    bench(32, ChatApp(env, cmd))
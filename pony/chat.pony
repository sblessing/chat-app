use "cli"
use "collections"
use "time"
use "random"
use "util"
use "math"
use "format"
use "term"
use "assert"

type ClientSeq is Array[Client]
type ChatSeq is Array[Chat]

primitive Post
primitive PostDelivery
primitive Leave
primitive Invite
primitive Compute
primitive Ignore
primitive Error

type Action is
  ( Post
  | PostDelivery
  | Leave
  | Invite
  | Compute
  | Ignore
  | Error
  | None
  )

type ActionMap is MapIs[Action, U64]

class val BehaviorFactory
  let _compute: U32
  let _post: U32
  let _leave: U32
  let _invite: U32

  new create(compute: U32, post: U32, leave: U32, invite: U32) =>
    _compute = compute
    _post = _compute + post
    _leave = _post + leave
    _invite = _leave + invite

  fun box apply(dice: DiceRoll): (Action | None) =>
    let pick = dice()
    var action: (Action | None) = None

    if pick < _compute then
      action = Compute
    elseif pick < _post then
      action = Post
    elseif pick < _leave then
      action = Leave
    elseif pick < _invite then
      action = Invite
    end

    action

actor Chat
  let _members: ClientSeq
  var _buffer: Array[(Array[U8] val | None)]

  new create(initiator: Client) =>
    _members = ClientSeq
    _buffer =  Array[(Array[U8] val | None)]

    _members.push(initiator)

  be post(payload: (Array[U8] val | None), accumulator: Accumulator) =>
    ifdef "_BENCH_NO_BUFFERED_CHATS" then
      None
    else
      _buffer.push(payload)
    end

    if _members.size() > 0 then
      // In distributed settings, which are not Pony,
      // there is a race between bump and forward. Take
      // care!
      accumulator.bump(Post, _members.size())

      for member in _members.values() do
        member.forward(this, payload, accumulator)
      end
    else
      accumulator.stop(Post)
    end

  be join(client: Client, accumulator: Accumulator) =>
    _members.push(client)

    ifdef not "_BENCH_NO_BUFFERED_CHATS" then
      if _buffer.size() > 0 then
        accumulator.bump(Ignore, _buffer.size())

        for message in _buffer.values() do
          client.forward(this, message, accumulator)
        end
      end
    end

    client.accepted(this, accumulator)

  be leave(client: Client, did_logout: Bool, accumulator: (Accumulator | None)) =>
    for (i, c) in _members.pairs() do
      if c is client then
        _members.remove(i, 1) ; break
      end
    end

    client.left(this, did_logout, accumulator)

actor Client
  let _id: U64
  let _friends: ClientSeq
  let _chats: ChatSeq
  let _directory: Directory
  let _dice: DiceRoll
  let _rand: SimpleRand
  var _fib_index: U8

  new create(id: U64, directory: Directory, seed: U64) =>
    _id = id
    _friends = ClientSeq
    _chats = ChatSeq
    _directory = directory
    _rand = SimpleRand(seed)
    _dice = DiceRoll(_rand)
    _fib_index = 35

  be befriend(client: Client) =>
    _friends.push(client)

  be logout() =>
    for chat in _chats.values() do
      chat.leave(this, true, None)
    else
      _directory.left(this)
    end

  be left(chat: Chat, did_logout: Bool, accumulator: (Accumulator | None)) =>
    for (i, c) in _chats.pairs() do
      if c is chat then
        _chats.remove(i, 1) ; break
      end
    end

    if ( _chats.size() == 0 ) and did_logout then
      _directory.left(this)
    else
      match accumulator
      | let accumulator': Accumulator => accumulator'.stop(Leave)
      end
    end

  be accepted(chat: Chat, accumulator: Accumulator) =>
    _chats.push(chat)
    accumulator.stop(Ignore)

  be forward(chat: Chat, payload: (Array[U8] val | None), accumulator: Accumulator) =>
    accumulator.stop(PostDelivery)

  be act(behavior: BehaviorFactory, accumulator: Accumulator) =>
    let index = _rand.nextInt(_chats.size().u32()).usize()

    try
      match behavior(_dice)
      | Post => _chats(index)?.post(None, accumulator)
      | Leave => _chats(index)?.leave(this, false, accumulator)
      | Compute => 
        for i in Range[U64](0, 10000) do
          if Fibonacci(_fib_index) != 9_227_465 then
            // This never happens, but makes it impossible for LLVM
            // to opt-out this entire loop.
            accumulator.stop(Error)
            _fib_index = _fib_index + 1
          end
        end

        accumulator.stop(Compute)
      | Invite =>
        let created = Chat(this)

        _chats.push(created)
        _rand.shuffle[Client](_friends)

        var invitations: USize = _rand.nextInt(_friends.size().u32()).usize()

        if invitations == 0 then
          invitations = 1
        end
          
        for k in Range[USize](0, invitations) do
          created.join(_friends(k)?, accumulator)
        end

        accumulator.bump(Invite, invitations)
      else
        // cannot happen
        Fact(false)?
      end
    else
      //might be because no friends or no chats
      accumulator.stop(None)
    end

actor Directory
  let _clients: ClientSeq
  let _random: SimpleRand
  let _befriend: U32
  var _poker: (Poker | None)

  new create(seed: U64, befriend': U32) =>
    _clients = ClientSeq
    _random = SimpleRand(seed)
    _befriend = befriend'
    _poker = None

  be login(id: U64) =>
    _clients.push(Client(id, this, _random.next()))

  be befriend() =>
    var befriend' = false

    for friend in _clients.values() do
      befriend' = false
      
      while not befriend' do
        for client in _clients.values() do
          if (_random.nextInt(100) < _befriend) and (friend isnt client) then
            client.befriend(friend)
            friend.befriend(client)
            befriend' = true
          end
        end
      end
    end

  be left(client: Client) =>
    for (i, c) in _clients.pairs() do
      if c is client then
        _clients.remove(i, 1) ; break
      end
    end

    if _clients.size() == 0 then
      match _poker
      | let poker: Poker => poker.finished()
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

  new start(poker: Poker, expected: USize) =>
    _poker = poker
    _actions = recover ActionMap end
    _start = Time.millis().f64()
    _end = 0
    _duration = 0
    _expected = expected
    _did_stop = false

  fun ref _count(action: Action) =>
    try
      _actions(action) = _actions(action)? + 1
    else
      _actions(action) = 1
    end

  be bump(action: Action, expected: USize) =>
    _count(action)
    _expected = ( _expected + expected ) - 1

  be stop(action: Action = Ignore) =>
    _count(action)
    _expected = _expected - 1

    if _expected == 0 then
      _end = Time.millis().f64()
      _duration = _end - _start
      _did_stop = true

      _poker.confirm()
    end

   be print(poker: Poker, i: USize, j: USize) =>
     poker.collect(i, j, _duration, _actions = recover ActionMap end)

actor Poker
  let _actions: ActionMap
  let _parseable: Bool
  let _directory_count: USize
  var _clients: U64
  var _logouts: USize
  var _confirmations: USize
  var _turns: U64
  var _befriend: U32
  var _iteration: USize
  var _directories: Array[Directory] val
  var _runtimes: Array[Accumulator]
  var _accumulations: USize
  var _finals: Array[Array[F64]]
  var _factory: BehaviorFactory
  var _bench: (AsyncBenchmarkCompletion | None)
  var _last: Bool
  var _turn_series: Array[F64]

  new create(parseable: Bool, clients: U64, turns: U64, directories: USize, befriend: U32, factory: BehaviorFactory) =>
    _actions = ActionMap
    _parseable = parseable
    _directory_count = directories
    _clients = clients
    _logouts = 0
    _confirmations = 0
    _turns = turns
    _befriend = befriend
    _iteration = 0
    _directories = recover val Array[Directory] end
    _runtimes = Array[Accumulator]
    _accumulations = 0
    _finals = Array[Array[F64]]
    _factory = factory
    _bench = None
    _last = false
    _turn_series = Array[F64]

  fun ref _prepare() =>
    let rand = SimpleRand(42)

    _directories = recover
      let dirs = Array[Directory](_directory_count)

      for i in Range[USize](0, _directory_count) do
        dirs.push(Directory(rand.next(), _befriend))
      end

      dirs
    end

  be apply(bench: AsyncBenchmarkCompletion, last: Bool) =>
    _prepare()

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

    // To make sure that nobody's friendset is empty
    if _befriend > 0 then
      for directory in _directories.values() do
        directory.befriend()
      end
    end

    while ( turns = turns - 1 ) >= 1 do
      let accumulator = Accumulator.start(this, _clients.usize())

      for directory in _directories.values() do
        directory.poke(_factory, accumulator)
      end

      _runtimes.push(accumulator)
    end

  be confirm() =>
    _confirmations = _confirmations - 1
    if _confirmations == 0 then
      for d in _directories.values() do
        d.disconnect(this)
      end
    end

  be finished() =>
    _logouts = _logouts - 1
    if _logouts == 0 then
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

    _accumulations = _accumulations - 1
    if _accumulations == 0 then
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

          let quality_of_service: String val = SampleStats(qos = Array[F64]).median().string()

          if not _parseable then
            bench.append(
              "".join([
                ANSI.bold()
                Format("" where width = 31)
                Format("j-mean" where width = 18, align = AlignRight)
                Format("j-median" where width = 18, align = AlignRight)
                Format("j-error" where width = 18, align = AlignRight)
                Format("j-stddev" where width = 18, align = AlignRight)
                Format("quality of service" where width = 32, align = AlignRight)
                ANSI.reset()
              ].values())
            )

            bench.append(
              "".join([
                Format("Turns" where width = 31)
                Format(stats.mean().string() + " ms" where width = 18, align = AlignRight)
                Format(stats.median().string() + " ms" where width = 18, align = AlignRight)
                Format("±" + stats.err().string() + " %" where width = 18, align = AlignRight)
                Format(stats.stddev().string() where width = 18, align = AlignRight)
                Format(quality_of_service where width = 32, align = AlignRight)
              ].values())
            )

            bench.append("")
          else
            bench.append(
              ",".join([
                "Turns"
                stats.mean().string()
                stats.median().string()
                stats.err().string()
                stats.stddev().string()
                quality_of_service
              ].values())
            )
          end         

          for (key, value) in _actions.pairs() do
            // could make 'Actions' stringable
            let identifier =
              match key
              | Post         => "Post"
              | PostDelivery => "PostDelivery"
              | Leave        => "Leave"
              | Invite       => "Invite"
              | Compute      => "Compute"
              | Ignore       => "Ignore"
              | Error        => "Error"
              | None         => "None"
              end

            if not _parseable then
              bench.append(
                "".join([
                    Format(identifier where width = 16)
                    Format(value.string() where width = 10, align = AlignRight)
                  ].values()
                )
              )
            else
              bench.append(
                ",".join([
                  identifier
                  value.string()
                ].values())
              )
            end
          end
        end
      end
    end

class iso ChatApp is AsyncActorBenchmark
  var _clients: U64
  var _turns: U64
  var _factory: BehaviorFactory val
  var _poker: Poker
  var _invalid_args: Bool

  new iso create(env: Env, cmd: Command val) =>
    _clients = cmd.option("clients").u64()
    _turns = cmd.option("turns").u64()
    _invalid_args = false

    let directories: USize = cmd.option("directories").u64().usize()
    let compute: U32 = cmd.option("compute").u64().u32()
    let post: U32 = cmd.option("post").u64().u32()
    let leave: U32 = cmd.option("leave").u64().u32()
    let invite: U32 = cmd.option("invite").u64().u32()
    let befriend: U32 = cmd.option("befriend").u64().u32()
    let parseable: Bool = cmd.option("parseable").bool()

    let sum = compute + post + leave + invite

    _invalid_args  =
      if (sum != 100) or ((befriend == 0) and (invite > 0)) then
        env.out.print("Invalid arguments: sum != 0 or befriend == 0 and invite > 0!")
        env.exitcode(-1)
        true
      else
        false
      end

    _factory = recover BehaviorFactory(compute, post, leave, invite) end

    _poker = Poker(parseable, _clients, _turns, directories, befriend, _factory)

  fun box apply(c: AsyncBenchmarkCompletion, last: Bool) => 
    if _invalid_args == false then
      _poker(c, last)
    else
      c.abort()
    end

  fun tag name(): String => "Chat App"

actor Main is BenchmarkRunner
  new create(env: Env) =>
    try
      let cs =
        recover
          CommandSpec.leaf("chat-app", "Cross Language Actor Benchmark", [
            OptionSpec.u64("iterations", "The number of iterations to execute. Defaults to 32."
              where short' = 'r', default' = U64(32))
            OptionSpec.u64("clients", "The number of clients. Defaults to 1024."
              where short' = 'c', default' = U64(1024))
            OptionSpec.u64("directories", "The number of directories. Defaults to 8."
              where short' = 'd', default' = U64(8))
            OptionSpec.u64("turns", "The number of turns. Defaults to 32."
              where short' = 't', default' = U64(32))
            OptionSpec.u64("compute", "The compute behavior probability. Defaults to 55."
              where short' = 'm', default' = U64(55))
            OptionSpec.u64("post", "The post behavior probability. Defaults to 25."
              where short' = 'p', default' = U64(25))
            OptionSpec.u64("leave", "The leave behavior probability. Defaults to 10."
              where short' = 'l', default' = U64(10))
            OptionSpec.u64("invite", "The invite behavior probability. Defaults to 10."
              where short' = 'i', default' = U64(10))
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
    let iterations: U64 = cmd.option("iterations").u64()

    bench(iterations, ChatApp(env, cmd))

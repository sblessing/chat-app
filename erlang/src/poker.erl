%%%-------------------------------------------------------------------
%%% @author Rudolf Schlatte <rudi@constantly.at>
%%% @copyright (C) 2020, Rudolf Schlatte
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2020 by Rudolf Schlatte <rudi@constantly.at>
%%%-------------------------------------------------------------------
-module(poker).

-behaviour(gen_statem).

%% API
-export([start/9]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
%% state functions
-export([idle/3, running/3]).

%% chat-app events

%% Events sent by main
-export([apply/2,sync/1]).
%% Events sent by accumulator
-export([confirm/2]).

-define(SERVER, ?MODULE).

-record(data, {
               %% Per-iteration simulation parameters
               n_clients, n_directories, n_turns, p_compute, p_post, p_leave, p_invite, p_befriend, parseable,
               %% Internal state
               directories, outstanding_turns
              }).

%%%===================================================================
%%% API
%%%===================================================================

%% "The poker drives the benchmark and is activated by the main entity once
%% per iteration."
apply(Poker, Iteration) ->
    gen_statem:call(Poker, {apply, Iteration}).

%% This returns when the poker is idle; to be called after the last call to
%% `apply' to make sure the last iteration terminates
sync(Poker) ->
    gen_statem:call(Poker, sync).

confirm(Poker, Accumulator) ->
    gen_statem:cast(Poker, {confirm, Accumulator}).

idle({call, From}, {apply, Iteration},
     Data=#data{n_turns=Turns,directories=Directories,n_directories=NDirectories,n_clients=NClients}) ->
    io:format("Poker starting iteration ~w~n", [Iteration]),
    %% Create clients
    lists:foreach(fun(I) -> directory:login(lists:nth(I rem NDirectories + 1, Directories), I) end,
                  lists:seq(1, NClients)),
    %% Initiate befriend phase
    lists:foreach(fun(D) -> directory:befriend(D) end, Directories),
    %% Send n turns (all at once, no waiting)
    lists:foreach(fun(Turn) ->
                          {ok, Accumulator}=accumulator:start(self(), Iteration, Turn, NClients),
                          lists:foreach(fun(D) -> directory:poke(D, Turn, Accumulator) end, Directories)
                  end,
                  lists:seq(1, Turns)),
    {next_state, running, Data#data{outstanding_turns=Turns}, [{reply, From, ok}]};
idle({call, From}, sync, _Data) ->
    {keep_state_and_data, [{reply, From, ok}]}.



running({call, _From}, {apply, Iteration}, _Data) ->
    %% Delay next iteration until previous one finished
    io:format("Poker postponing iteration ~w~n", [Iteration]),
    {keep_state_and_data, [postpone]};
running({call, _From}, sync, _Data) ->
    {keep_state_and_data, [postpone]};
running(cast, {confirm, _Accumulator},
        Data=#data{outstanding_turns=Turns,directories=Directories}) ->
    %% Accept out-of-order confirmation of accumulators; only keep track how
    %% many turns still outstanding
    case Turns of
        1 ->
            io:format("Poker finishing iteration~n"),
            lists:foreach(fun directory:disconnect/1, Directories),
            {next_state, idle, Data#data{outstanding_turns=0}};
        _ -> {keep_state, Data#data{outstanding_turns=Turns - 1}}
    end.


start(Clients, Directories, Turns, Compute, Post, Leave, Invite, Befriend, Parseable) ->
    gen_statem:start({local, ?SERVER}, ?MODULE,
                     [Clients, Directories, Turns, Compute, Post, Leave, Invite, Befriend, Parseable],
                     []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Clients, Directories, Turns, Compute, Post, Leave, Invite, Befriend, Parseable]) ->
    io:format("Poker creating ~w directories~n", [Directories]),
    D=lists:map(fun(I) -> {ok, Dir}=directory:start(I, Compute, Post, Leave, Invite, Befriend), Dir end,
                lists:seq(1, Directories)),
    {ok, idle, #data{
                  n_clients=Clients,
                  n_directories=Directories,
                  n_turns=Turns,
                  p_compute=Compute,
                  p_post=Post,
                  p_leave=Leave,
                  p_invite=Invite,
                  p_befriend=Befriend,
                  parseable=Parseable,
                  directories=D
                 }}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

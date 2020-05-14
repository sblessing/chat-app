%%%-------------------------------------------------------------------
%%% @author Rudolf Schlatte <rudi@constantly.at>
%%% @copyright (C) 2020, Rudolf Schlatte
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2020 by Rudolf Schlatte <rudi@constantly.at>
%%%-------------------------------------------------------------------
-module(accumulator).

-behaviour(gen_statem).

%% API
-export([start/4]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([running/3]).

%% chat-app events
-export([stop/2,bump/2]).

-record(data, {start_time,count=0,poker,iteration,turn}).

%%%===================================================================
%%% API
%%%===================================================================

start(Poker, Iteration, Turn, NClients) ->
    gen_statem:start(?MODULE, [Poker, Iteration, Turn, NClients], []).


bump(Accumulator, Amount) ->
    gen_statem:call(Accumulator, {bump, Amount}).

stop(Accumulator, Action) ->
    gen_statem:cast(Accumulator, {stop, Action}).


running({call, From}, {bump, Amount}, Data=#data{count=Count,turn=_Turn}) ->
    {keep_state, Data#data{count=Count + Amount}, {reply, From, ok}};
running(cast, {stop, Action}, Data=#data{start_time=StartTime,poker=Poker,count=Count,iteration=Iteration,turn=Turn}) ->
    case Count of
        0 ->
            %% This is for diagnostic purposes; change the return value of the
            %% `1' case below to activate this branch
            %%io:format("*** Accumulator for iteration ~w turn ~w got surplus stop signal with action ~w~n", [Iteration, Turn, Action]),
            keep_state_and_data;
        1 ->
            %%io:format("Iteration ~w turn ~w finished in ~w ms~n", [Iteration, Turn, erlang:system_time(millisecond) - StartTime]),
            poker:confirm(Poker, self()),
            {stop, normal};
            %% {keep_state, Data#data{count=0}};
        _ ->
            {keep_state, Data#data{count=Count - 1}}
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Poker, Iteration, Turn, NClients]) ->
    %% io:format("Accumulator starting for iteration ~w, turn ~w~n", [Iteration, Turn]),
    {ok, running,
     #data{start_time=erlang:system_time(millisecond), count=NClients, poker=Poker, iteration=Iteration, turn=Turn}}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

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
-export([start/2]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([running/3]).

%% chat-app events
-export([stop/1,bump/2]).

-record(data, {count=0,poker,turn}).

%%%===================================================================
%%% API
%%%===================================================================

start(Poker, Turn) ->
    gen_statem:start(?MODULE, [Poker, Turn], []).


bump(Accumulator, Amount) ->
    gen_statem:cast(Accumulator, {bump, Amount}).

stop(Accumulator) ->
    gen_statem:cast(Accumulator, stop).


running(cast, {bump, Amount}, Data=#data{count=Count,turn=Turn}) ->
    %% io:format("Accumulator for turn ~w getting bumped by ~w at ~w~n", [Turn, Amount, Count]),
    {keep_state, Data#data{count=Count + Amount}};
running(cast, stop, Data=#data{poker=Poker,count=Count,turn=Turn}) ->
    case Count of
        1 ->
            %% io:format("Accumulator for turn ~w stopped at ~w, sending confirmation to poker~n", [Turn, Count]),
            poker:confirm(Poker, self()),
            {stop, normal};
        _ ->
            %% io:format("Accumulator for turn ~w getting stopped at ~w~n", [Turn, Count]),
            {keep_state, Data#data{count=Count - 1}}
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Poker, Turn]) ->
    process_flag(trap_exit, true),
    {ok, running, #data{count=0,poker=Poker,turn=Turn}}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

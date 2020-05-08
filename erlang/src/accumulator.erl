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
-export([start/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([running/3]).

%% chat-app events
-export([stop/1,bump/2]).

-record(data, {count=0,poker}).

%%%===================================================================
%%% API
%%%===================================================================

start(Poker) ->
    gen_statem:start(?MODULE, [Poker], []).


bump(Accumulator, Amount) ->
    gen_statem:cast(Accumulator, {bump, Amount}).

stop(Accumulator) ->
    gen_statem:cast(Accumulator, stop).


running(cast, {bump, Amount}, Data=#data{count=Count}) ->
    {keep_state, Data#data{count=Count + Amount}};
running(cast, stop, Data=#data{poker=Poker,count=Count}) ->
    case Count of
        1 ->
            poker:confirm(Poker, self()),
            {stop, normal};
        _ ->
            {keep_state, Data#data{count=Count - 1}}
    end.

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Poker]) ->
    process_flag(trap_exit, true),
    {ok, running, #data{count=0,poker=Poker}}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

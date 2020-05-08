%%%-------------------------------------------------------------------
%%% @author Rudolf Schlatte <rudi@constantly.at>
%%% @copyright (C) 2020, Rudolf Schlatte
%%% @doc
%%%
%%% @end
%%% Created :  8 May 2020 by Rudolf Schlatte <rudi@constantly.at>
%%%-------------------------------------------------------------------
-module(directory).

-behaviour(gen_statem).

%% API
-export([start_link/1]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
%% state functions
-export([running/3]).

%% chat-app events
-export([login/1,befriend/1,poke/3]).

-record(data, {id}).

%%%===================================================================
%%% API
%%%===================================================================

login(Directory) ->
    ok.

befriend(Directory) ->
    %% non-blocking (use cast)
    ok.

%% Starts a turn
poke(Directory, Turn, Accumulator) ->
    gen_statem:cast(Directory, {poke, Turn, Accumulator}).

running(cast, {poke, Turn, Accumulator}, Data) ->
    accumulator:bump(Accumulator, 1),
    accumulator:stop(Accumulator),
    keep_state_and_data.


start_link(Id) ->
    gen_statem:start_link(?MODULE, [Id], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Id]) ->
    process_flag(trap_exit, true),
    io:format("Directory ~w starting~n", [Id]),
    {ok, running, #data{id=Id}}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

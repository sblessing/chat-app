%%%-------------------------------------------------------------------
%%% @author Rudolf Schlatte <rudi@constantly.at>
%%% @copyright (C) 2020, Rudolf Schlatte
%%% @doc
%%%
%%% @end
%%% Created : 10 May 2020 by Rudolf Schlatte <rudi@constantly.at>
%%%-------------------------------------------------------------------
-module(client).

-behaviour(gen_statem).

%% API
-export([start/5]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([running/3]).

%% Events sent by directory
-export([befriend/2,act/2,logout/1]).

-record(data, {id,accumulator,friends=[],chats=[],
               p_compute,
               p_post,
               p_leave,
               p_invite
              }).

%%%===================================================================
%%% API
%%%===================================================================

logout(Client) ->
    gen_statem:cast(Client, logout).

befriend(Client, Friend) ->
    gen_statem:cast(Client, {befriend, Friend}).

act(Client,Accumulator) ->
    gen_statem:cast(Client, {act,Accumulator}).

start(Id,Compute,Post,Leave,Invite) ->
    gen_statem:start(?MODULE, [Id,Compute,Post,Leave,Invite], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Id,Compute,Post,Leave,Invite]) ->
    %% io:format("Client ~w starting~n", [Id]),
    {ok, running, #data{id=Id,
                        p_compute=Compute,
                        p_post=Post,
                        p_leave=Leave,
                        p_invite=Invite
                       }}.

running(cast, {befriend, Friend}, Data=#data{friends=Friends}) ->
    {keep_state, Data#data{friends=[Friend | Friends]}};
running(cast, {act, Accumulator},
        _Data=#data{id=Id,friends=Friends,chats=Chats,
                    p_compute=Compute, p_post=Post, p_leave=Leave, p_invite=_Invite}) ->
    Act=rand:uniform(100),
    case Act of
        _ when Act < Compute ->
            io:format("Client ~w starting a compute~n", [Id]),
            Result=compute(),
            io:format("Client ~w did a compute with result ~w~n", [Id, Result]),
            accumulator:stop(Accumulator);
        _ when Act < Compute + Post ->
            io:format("Client ~w doing a post~n", [Id]),
            case Chats of
                [] -> ok;                       % bump accumulator
                _ -> ok                         % send msg to random chat
            end,
            accumulator:stop(Accumulator);
        _ when Act < Compute + Post + Leave ->
            io:format("Client ~w doing a leave~n", [Id]),
            ok,                        % leave action here
            accumulator:stop(Accumulator);
        _ ->                           % Compute + Post + Leave + Invite = 100
            io:format("Client ~w doing a invite~n", [Id]),
            ok,                         % invite action here
            accumulator:stop(Accumulator)
    end,
    keep_state_and_data;
running(cast, logout, _Data=#data{id=Id}) ->
    %% io:format("Client ~w being told to logout~n", [Id]),
    {stop, normal}.

terminate(_Reason, _State, _Data=#data{id=Id}) ->
    %% io:format("Client ~w stopping~n", [Id]),
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

compute() ->
    fibonacci(35).

fibonacci(0) ->
    0;
fibonacci(1) ->
    1;
fibonacci(N) ->
    fibonacci(N-1) + fibonacci(N-2).


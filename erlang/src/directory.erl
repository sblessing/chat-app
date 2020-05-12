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
-export([start/6]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
%% state functions
-export([running/3]).

%% Events sent by poker
-export([login/2,befriend/1,poke/3,disconnect/1]).

-record(data, {id,
               clients=[],
               p_compute,
               p_post,
               p_leave,
               p_invite,
               p_befriend
              }).

%%%===================================================================
%%% API
%%%===================================================================

login(Directory, ClientId) ->
    gen_statem:cast(Directory, {login, ClientId}).

befriend(Directory) ->
    gen_statem:cast(Directory, befriend).

%% Starts a turn
poke(Directory, Turn, Accumulator) ->
    gen_statem:cast(Directory, {poke, Turn, Accumulator}).

%% Disconnects directory after an iteration (stops clients)
disconnect(Directory) ->
    gen_statem:cast(Directory, disconnect).

running(cast, {login, ClientId},
        Data=#data{clients=Clients,p_compute=Compute, p_post=Post, p_leave=Leave, p_invite=Invite}) ->
    {ok, Client} = client:start(ClientId,Compute,Post,Leave,Invite),
    {keep_state, Data#data{clients=[ Client | Clients ] }};
running(cast, befriend, _Data=#data{clients=Clients,p_befriend=PBefriend}) ->
    lists:foreach(
      fun(C) ->
              %% Make sure that each client has at least one friend.
              FirstFriendIndex=rand:uniform(length(Clients)-1),
              FirstFriend=case FF=lists:nth(FirstFriendIndex, Clients) of
                              C -> lists:nth(FirstFriendIndex+1, Clients);
                              _ -> FF
                          end,
              client:befriend(C, FirstFriend),
              client:befriend(FirstFriend, C),
              lists:foreach(
                fun(F) -> case C /= F andalso rand:uniform(100) < PBefriend of
                              true -> client:befriend(C, F),
                                      client:befriend(F, C);
                              _ -> ok
                          end
                end,
                Clients)
      end,
      Clients),
    keep_state_and_data;
running(cast, {poke, Turn, Accumulator}, _Data=#data{id=_Id,clients=Clients}) ->
    io:format("Directory ~w getting poked for turn ~w~n", [_Id, Turn]),
    lists:foreach(fun(C) -> client:act(C, Turn, Accumulator) end, Clients),
    keep_state_and_data;
running(cast, disconnect, Data=#data{id=Id,clients=Clients}) ->
    io:format("Directory ~w disconnecting after iteration complete~n", [Id]),
    lists:foreach(fun client:logout/1, Clients),
    {keep_state, Data#data{clients=[]}}.


start(Id, Compute, Post, Leave, Invite, Befriend) ->
    gen_statem:start_link(?MODULE, [Id, Compute, Post, Leave, Invite, Befriend], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Id, Compute, Post, Leave, Invite, Befriend]) ->
    process_flag(trap_exit, true),
    io:format("Directory ~w starting~n", [Id]),
    {ok, running, #data{id=Id,
                        p_compute=Compute,
                        p_post=Post,
                        p_leave=Leave,
                        p_invite=Invite,
                        p_befriend=Befriend}}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

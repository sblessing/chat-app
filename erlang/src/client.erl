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
-export([befriend/2,act/3,logout/1]).

%% Events sent by chat
-export([forward/3,left/3]).

-record(data, {id,accumulator,
               %% an ordset is a list that is kept ordered, therefore we don’t
               %% break the benchmark rules
               friends=ordsets:new(),
               chats=[],
               p_compute,
               p_post,
               p_leave,
               p_invite
              }).

%%%===================================================================
%%% API
%%%===================================================================

start(Id,Compute,Post,Leave,Invite) ->
    gen_statem:start(?MODULE, [Id,Compute,Post,Leave,Invite], []).

logout(Client) ->
    gen_statem:cast(Client, logout).

befriend(Client, Friend) ->
    gen_statem:cast(Client, {befriend, Friend}).

act(Client, Turn, Accumulator) ->
    gen_statem:cast(Client, {act, Turn, Accumulator}).

forward(Client, Message, Accumulator) ->
    gen_statem:cast(Client, {forward, Message, Accumulator}).

left(Client, Chat, Accumulator) ->
    gen_statem:cast(Client, {left, Chat, Accumulator}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Id,Compute,Post,Leave,Invite]) ->
    {ok, running, #data{id=Id,
                        p_compute=Compute,
                        p_post=Post,
                        p_leave=Leave,
                        p_invite=Invite
                       }}.

running(cast, {befriend, Friend}, Data=#data{friends=Friends}) ->
    %% an ordset is a list that is kept ordered, therefore we don’t break the
    %% benchmark rules
    {keep_state, Data#data{friends=ordsets:add_element(Friend, Friends)}};
running(cast, {act, Turn, Accumulator},
        Data=#data{id=Id,friends=Friends,chats=Chats,
                   p_compute=Compute, p_post=Post, p_leave=Leave, p_invite=_Invite}) ->
    Act=rand:uniform(100),
    case Act of
        _ when Act < Compute ->
            %% Compute
            compute(),
            accumulator:stop(Accumulator, {client, compute}),
            keep_state_and_data;
        _ when Act < Compute + Post ->
            %% Post
            case Chats of
                [] -> accumulator:stop(Accumulator, {client, post});
                _ -> chat:post(lists:nth(rand:uniform(length(Chats)), Chats), self(), {Id, Turn}, Accumulator)
            end,
            keep_state_and_data;
        _ when Act < Compute + Post + Leave ->
            %% Leave
            case Chats of
                [] -> accumulator:stop(Accumulator, {client, leave, empty});
                _ ->
                    %% the chat will call back with a `left' message; state
                    %% update and accumulator:stop are done there
                    chat:leave(lists:nth(rand:uniform(length(Chats)), Chats), self(), Accumulator)
                 end,
            keep_state_and_data;
        _ ->                           % Compute + Post + Leave + Invite = 100
            %% Invite
            {ok, Chat}=chat:start(self()),
            %% pony shuffles the friends list and takes a prefix of random
            %% length, we take each element with 50% probability
            Inviteds=case L=lists:filter(fun(_) -> rand:uniform(2) == 1 end, Friends) of
                         [] -> [lists:nth(rand:uniform(length(Friends)), Friends)];
                         _ -> L
                     end,
            accumulator:bump(Accumulator, length(Inviteds)),
            lists:foreach(fun(I) -> chat:join(Chat, I, Accumulator) end, Inviteds),
            accumulator:stop(Accumulator, {client, invite}),
            {keep_state, Data#data{chats=[Chat | Chats]}}
    end;
running(cast, {forward, _Message, Accumulator}, _Data=#data{id=_Id}) ->
    accumulator:stop(Accumulator, {client, forward}),
    keep_state_and_data;
running(cast, {left, Chat, Accumulator}, Data=#data{chats=Chats}) ->
    accumulator:stop(Accumulator, {client, left}),
    { keep_state, Data#data{chats=lists:delete(Chat, Chats)} };
running(cast, logout, _Data=#data{id=_Id}) ->
    {stop, normal}.

terminate(_Reason, _State, _Data=#data{id=_Id}) ->
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


%%%-------------------------------------------------------------------
%%% @author Rudolf Schlatte <rudi@constantly.at>
%%% @copyright (C) 2020, Rudolf Schlatte
%%% @doc
%%%
%%% @end
%%% Created : 12 May 2020 by Rudolf Schlatte <rudi@constantly.at>
%%%-------------------------------------------------------------------
-module(chat).

-behaviour(gen_statem).

%% API
-export([start/1]).

%% Called by client
-export([join/3,post/4,leave/3]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([running/3]).

-define(SERVER, ?MODULE).

-record(data, {members=[],buffer=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start(Initiator) ->
    gen_statem:start(?MODULE, [Initiator], []).

join(Chat, Client, Accumulator) ->
    gen_statem:cast(Chat, {join, Client, Accumulator}).

post(Chat, Client, Message, Accumulator) ->
    gen_statem:cast(Chat, {post, Client, Message, Accumulator}).

leave(Chat, Client, Accumulator) ->
    gen_statem:cast(Chat, {leave, Client, Accumulator}).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

callback_mode() -> state_functions.

init([Initiator]) ->
    {ok, running, #data{members=[Initiator]}}.

running(cast, {join, Client, Accumulator}, Data=#data{members=Members,buffer=Buffer}) ->
    case Buffer of
        [] -> ok;
        _ ->
            accumulator:bump(Accumulator, length(Buffer)),
            lists:foreach(fun(M) -> client:forward(Client, M, Accumulator) end, Buffer)
    end,
    accumulator:stop(Accumulator, {chat, join}),
    {keep_state, Data#data{members=[Client | Members]}};
running(cast, {post, _Client, Message, Accumulator},
        Data=#data{members=Members, buffer=Buffer}) ->
    case Members of
        [] -> ok;
        _ ->
            io:format("Chat forwarding message ~w to ~w clients~n", [Message, length(Members)]),
            accumulator:bump(Accumulator, length(Members)),
             lists:foreach(fun(C) -> client:forward(C, Message, Accumulator) end, Members)
    end,
    accumulator:stop(Accumulator, {chat, post}),
    { keep_state, Data#data{buffer=[Message | Buffer]} };
running(cast, {leave, Client, Accumulator}, Data=#data{members=Members}) ->
    client:left(Client, self(), Accumulator),
    { keep_state, Data#data{members=lists:delete(Client, Members)}}.


terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

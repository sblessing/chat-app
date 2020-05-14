-module(chatapp).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    OptSpecList =
        [
         {iterations, $r, "iterations", {integer, 32}, "Number of iterations to execute"},
         {clients, $c, "clients", {integer, 1024}, "Number of clients"},
         {directories, $d, "directories", {integer, 8}, "Number of directories"},
         {turns, $t, "turns", {integer, 32}, "Number of turns"},
         {compute, $m, "compute", {integer, 55}, "Compute behavior probability"},
         {post, $p, "post", {integer, 25}, "Post behavior probability"},
         {leave, $l, "leave", {integer, 10}, "Leave behavior probability"},
         {invite, $i, "invite", {integer, 10}, "Invite behavior probability"},
         {befriend, $b, "befriend", {integer, 10}, "Probability of befriending"},
         {parseable, $s, "parseable", {boolean, false}, "Generate parseable output"}
        ],
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, []}} ->
            %%io:format("Options: ~w~n", [Options]),
            {ok, Poker}=poker:start(proplists:get_value(clients,Options),
                                    proplists:get_value(directories,Options),
                                    proplists:get_value(turns,Options),
                                    proplists:get_value(compute,Options),
                                    proplists:get_value(post,Options),
                                    proplists:get_value(leave,Options),
                                    proplists:get_value(invite,Options),
                                    proplists:get_value(befriend,Options),
                                    proplists:get_value(parseable,Options)),
            lists:foreach(fun(I) -> poker:apply(Poker, I) end,
                          lists:seq(1, proplists:get_value(iterations,Options))),
            poker:sync(Poker),
            io:format("Main finishing.~n");
        _ ->
            getopt:usage(OptSpecList, "chatapp")
    end,
    %% erlang:halt(0).
    ok.

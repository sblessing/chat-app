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
    case Parsed=getopt:parse(OptSpecList, Args) of
        {ok, {Options, []}} ->
            io:format("Args: ~w~n", [Parsed]);
        _ ->
            getopt:usage(OptSpecList, "chatapp")
    end,
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

%% --- Day 12: JSAbacusFramework.io ---
%%
%% Santa's Accounting-Elves need help balancing the books after a
%% recent order. Unfortunately, their accounting software uses a
%% peculiar storage format. That's where you come in.
%%
%% They have a JSON document which contains a variety of things:
%% arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and
%% strings. Your first job is to simply find all of the numbers
%% throughout the document and add them together.
%%
%% For example:
%%
%% [1,2,3] and {"a":2,"b":4} both have a sum of 6.
%% [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
%% {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
%% [] and {} both have a sum of 0.
%%
%% You will not encounter any strings containing numbers.
%%
%% What is the sum of all numbers in the document?
-module(day12).
-compile([export_all]).

solve_part1() ->
    solve(input()).

solve(Input) ->
    case re:run(Input, "(-?\\d+)", [{capture, all_but_first, list}, global]) of
        {match, Matches} ->
            lists:sum([list_to_integer(M) || [M] <- Matches]);
        nomatch ->
            0
    end.

input() ->
    {ok, Input} = file:read_file("input/day12"),
    binary_to_list(Input).

-include_lib("eunit/include/eunit.hrl").

day12_test_() ->
    [ ?_assertEqual(6, solve("[1,2,3]"))
    , ?_assertEqual(6, solve("{\"a\":2,\"b\":4}"))
    , ?_assertEqual(0, solve("{\"a\":[-1,1]}"))
    , ?_assertEqual(0, solve("[]"))
    ].

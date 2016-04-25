%% --- Day 10: Elves Look, Elves Say ---
%%
%% Today, the Elves are playing a game called look-and-say. They take
%% turns making sequences by reading aloud the previous sequence and
%% using that reading as the next sequence. For example, 211 is read
%% as "one two, two ones", which becomes 1221 (1 2, 2 1s).
%%
%% Look-and-say sequences are generated iteratively, using the
%% previous value as input for the next step. For each step, take the
%% previous value, and replace each run of digits (like 111) with the
%% number of digits (3) followed by the digit itself (1).
%%
%% For example:
%%
%% 1 becomes 11 (1 copy of digit 1).
%% 11 becomes 21 (2 copies of digit 1).
%% 21 becomes 1211 (one 2 followed by one 1).
%% 1211 becomes 111221 (one 1, one 2, and two 1s).
%% 111221 becomes 312211 (three 1s, two 2s, and one 1).
%%
%% Starting with the digits in your puzzle input, apply this process
%% 40 times. What is the length of the result?
-module(day10).
-compile([export_all]).

solve_part1() ->
    length(solve(input(), 40)).

solve_part2() ->
    length(solve(input(), 50)).

input() ->
    "1113122113".

solve(Input, 0) -> Input;
solve([H|T], N) -> solve(solve(T, H, 1), N-1).

solve([], I, N)    -> [N+$0,I];
solve([I|T], I, N) -> solve(T, I, N+1);
solve([H|T], I, N) -> [N+$0,I|solve(T, H, 1)].

-include_lib("eunit/include/eunit.hrl").

day10_test_() ->
    [ ?_assertEqual("11",     solve("1", 1))
    , ?_assertEqual("21",     solve("11", 1))
    , ?_assertEqual("1211",   solve("21", 1))
    , ?_assertEqual("111221", solve("1211", 1))
    , ?_assertEqual("312211", solve("111221", 1))
    , ?_assertEqual("312211", solve("1", 5))
    ].

%% --- Day 17: No Such Thing as Too Much ---
%%
%% The elves bought too much eggnog again - 150 liters this time. To
%% fit it all into your refrigerator, you'll need to move it into
%% smaller containers. You take an inventory of the capacities of the
%% available containers.
%%
%% For example, suppose you have containers of size 20, 15, 10, 5, and
%% 5 liters. If you need to store 25 liters, there are four ways to do
%% it:
%%
%% 15 and 10
%% 20 and 5 (the first 5)
%% 20 and 5 (the second 5)
%% 15, 5, and 5
%%
%% Filling all containers entirely, how many different combinations of
%% containers can exactly fit all 150 liters of eggnog?

-module(day17).
-compile([export_all]).

solve_part1() ->
    length(solve(150, input())).

solve_part2() ->
    Solutions = solve(150, input()),
    Min = lists:min([length(S) || S <- Solutions]),
    length([S || S <- Solutions, length(S) =:= Min]).

input() ->
    {ok, Input} = file:read_file("input/day17"),
    lists:map(fun list_to_integer/1,
              string:tokens(binary_to_list(Input), "\n")).

solve(Volume, Buckets) ->
    solve(Volume, lists:sort(Buckets), []).

solve(0, _, Acc) ->
    [Acc];
solve(Volume, [H|T], Acc) when Volume >= H ->
    solve(Volume-H, T, [H|Acc]) ++ solve(Volume, T, Acc);
solve(_, _, _) ->
    [].


-include_lib("eunit/include/eunit.hrl").

day17_test() ->
    ?assertEqual(4, length(solve(25, [20, 5, 5, 10, 15]))).

part1_test() ->
    ?assertEqual(4372, solve_part1()).

part2_test() ->
    ?assertEqual(4, solve_part2()).

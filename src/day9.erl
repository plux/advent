%% --- Day 9: All in a Single Night ---
%%
%% Every year, Santa manages to deliver all of his presents in a single night.
%%
%% This year, however, he has some new locations to visit; his elves
%% have provided him the distances between every pair of locations. He
%% can start and end at any two (different) locations he wants, but he
%% must visit each location exactly once. What is the shortest
%% distance he can travel to achieve this?
%%
%% For example, given the following distances:
%%
%% London to Dublin = 464
%% London to Belfast = 518
%% Dublin to Belfast = 141
%% The possible routes are therefore:
%%
%% Dublin -> London -> Belfast = 982
%% London -> Dublin -> Belfast = 605
%% London -> Belfast -> Dublin = 659
%% Dublin -> Belfast -> London = 659
%% Belfast -> Dublin -> London = 605
%% Belfast -> London -> Dublin = 982
%%
%% The shortest of these is London -> Dublin -> Belfast = 605, and so
%% the answer is 605 in this example.
%%
%% What is the distance of the shortest route?

-module(day9).
-compile([export_all]).

solve_part1() ->
    solve(input(), fun lists:min/1).

solve_part2() ->
    solve(input(), fun lists:max/1).

input() ->
    {ok, Input} = file:read_file("input/day9"),
    string:tokens(binary_to_list(Input), "\n").

solve(Input, F) ->
    {Places, Distances} = parse(Input),
    F([distance(Perm, Distances) || Perm <- perms(Places)]).

distance([_], _) ->
    0;
distance([A,B|T], Dists) ->
    maps:get({A,B}, Dists) + distance([B|T], Dists).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

parse(Input) ->
    {Places, Distances} =
        lists:foldl(fun(Line, {Ps, Ds}) ->
                            [A, "to", B, "=", D0] = string:tokens(Line, " "),
                            D = list_to_integer(D0),
                            {[A,B|Ps], maps:put({B,A}, D, maps:put({A,B}, D, Ds))}
                    end, {[], #{}}, Input),
    {lists:usort(Places), Distances}.

-include_lib("eunit/include/eunit.hrl").

day9_test() ->
    Input = [ "London to Dublin = 464"
            , "London to Belfast = 518"
            , "Dublin to Belfast = 141"
            ],
    ?assertEqual(605, solve(Input, fun lists:min/1)).

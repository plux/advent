%% --- Day 3: Perfectly Spherical Houses in a Vacuum ---
%%
%% Santa is delivering presents to an infinite two-dimensional grid of
%% houses.
%%
%% He begins by delivering a present to the house at his starting
%% location, and then an elf at the North Pole calls him via radio and
%% tells him where to move next. Moves are always exactly one house to
%% the north (^), south (v), east (>), or west (<). After each move,
%% he delivers another present to the house at his new location.
%%
%% However, the elf back at the north pole has had a little too much
%% eggnog, and so his directions are a little off, and Santa ends up
%% visiting some houses more than once. How many houses receive at
%% least one present?
%%
%% For example:
%%
%% > delivers presents to 2 houses: one at the starting location, and
%% one to the east.  ^>v< delivers presents to 4 houses in a square,
%% including twice to the house at his starting/ending location.
%% ^v^v^v^v^v delivers a bunch of presents to some very lucky children
%% at only 2 houses.
%%
%% --- Part Two ---
%%
%% The next year, to speed up the process, Santa creates a robot
%% version of himself, Robo-Santa, to deliver presents with him.
%%
%% Santa and Robo-Santa start at the same location (delivering two
%% presents to the same starting house), then take turns moving based
%% on instructions from the elf, who is eggnoggedly reading from the
%% same script as the previous year.
%%
%% This year, how many houses receive at least one present?
%%
%% For example:
%%
%% ^v delivers presents to 3 houses, because Santa goes north, and
%% then Robo-Santa goes south.  ^>v< now delivers presents to 3
%% houses, and Santa and Robo-Santa end up back where they started.
%% ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one
%% direction and Robo-Santa going the other.

-module(day3).
-compile([export_all]).

solve_part1() ->
    solve_part1(input()).

solve_part2() ->
    solve_part2(input()).

input() ->
    {ok, Input} = file:read_file("input/day3"),
    binary_to_list(Input).

solve_part1(Input) ->
    Houses = walk(Input),
    length(lists:usort(Houses)).

solve_part2(Input) ->
    {Santa, RoboSanta, _} = divide(Input),
    Houses1 = walk(Santa),
    Houses2 = walk(RoboSanta),
    Houses  = Houses1 ++ Houses2,
    length(lists:usort(Houses)).

divide(Input) ->
    lists:foldl(fun(Dir, {A, B, N}) when N rem 2 =:= 1 -> {[Dir|A], B, N+1};
                   (Dir, {A, B, N}) when N rem 2 =:= 0 -> {A, [Dir|B], N+1}
                end, {[], [], 1}, Input).

walk(Input) ->
    lists:foldl(fun(Dir, Houses) ->
                        NewPos = new_pos(Dir, hd(Houses)),
                        [NewPos|Houses]
                end, [{0,0}], Input).

new_pos($<, {X, Y}) -> {X-1, Y};
new_pos($>, {X, Y}) -> {X+1, Y};
new_pos($^, {X, Y}) -> {X, Y-1};
new_pos($v, {X, Y}) -> {X, Y+1}.

update_houses(Houses, NewPos) ->
    [NewPos|Houses].

-include_lib("eunit/include/eunit.hrl").

day3_test_() ->
    [ ?_assertEqual(2,  solve_part1("^"))
    , ?_assertEqual(4,  solve_part1("^>v<"))
    , ?_assertEqual(2,  solve_part1("^v^v^v^v^v"))
    , ?_assertEqual(3,  solve_part2("^v"))
    , ?_assertEqual(3,  solve_part2("^>v<"))
    , ?_assertEqual(11, solve_part2("^v^v^v^v^v"))
    , ?_assertEqual({"aaaa", "bbbb", 9}, divide("abababab"))
    ].

%% --- Day 18: Like a GIF For Your Yard ---
%%
%% After the million lights incident, the fire code has gotten
%% stricter: now, at most ten thousand lights are allowed. You arrange
%% them in a 100x100 grid.
%%
%% Never one to let you down, Santa again mails you instructions on
%% the ideal lighting configuration. With so few lights, he says,
%% you'll have to resort to animation.
%%
%% Start by setting your lights to the included initial configuration
%% (your puzzle input). A # means "on", and a . means "off".
%%
%% Then, animate your grid in steps, where each step decides the next
%% configuration based on the current one. Each light's next state
%% (either on or off) depends on its current state and the current
%% states of the eight lights adjacent to it (including
%% diagonals). Lights on the edge of the grid might have fewer than
%% eight neighbors; the missing ones always count as "off".
%%
%% For example, in a simplified 6x6 grid, the light marked A has the
%% neighbors numbered 1 through 8, and the light marked B, which is on
%% an edge, only has the neighbors marked 1 through 5:
%%
%% 1B5...
%% 234...
%% ......
%% ..123.
%% ..8A4.
%% ..765.
%%
%% The state a light should have next is based on its current state
%% (on or off) plus the number of neighbors that are on:
%%
%% - A light which is on stays on when 2 or 3 neighbors are on, and
%%   turns off otherwise.
%% - A light which is off turns on if exactly 3 neighbors are on, and
%%   stays off otherwise.
%% - All of the lights update simultaneously; they all consider the same
%%   current state before moving to the next.
%%
%% Here's a few steps from an example configuration of another 6x6 grid:
%%
%% Initial state:
%% .#.#.#
%% ...##.
%% #....#
%% ..#...
%% #.#..#
%% ####..
%%
%% After 1 step:
%% ..##..
%% ..##.#
%% ...##.
%% ......
%% #.....
%% #.##..
%%
%% After 2 steps:
%% ..###.
%% ......
%% ..###.
%% ......
%% .#....
%% .#....
%%
%% After 3 steps:
%% ...#..
%% ......
%% ...#..
%% ..##..
%% ......
%% ......
%%
%% After 4 steps:
%% ......
%% ......
%% ..##..
%% ..##..
%% ......
%% ......
%% After 4 steps, this example has four lights on.
%%
%% In your grid of 100x100 lights, given your initial configuration,
%% how many lights are on after 100 steps?

-module(day18).
-compile([export_all]).

solve_part1() ->
    init(input()),
    [next_state(N) || N <- lists:seq(0, 99)],
    count_turned_on(100).

solve_part2() ->
    init(input()),
    corners_on(0),
    [begin
         next_state(N),
         corners_on(N+1)
     end || N <- lists:seq(0, 99)],
    count_turned_on(100).

corners_on(N) ->
    put({0 ,  0, N}, on),
    put({0 , 99, N}, on),
    put({99,  0, N}, on),
    put({99, 99, N}, on).

next_state(N) ->
    [update({X,Y,N}) || X <- lists:seq(0, 99),
                        Y <- lists:seq(0, 99)].

count_turned_on(N) ->
    lists:sum([on({X,Y,N}) || X <- lists:seq(0, 99),
                              Y <- lists:seq(0, 99)]).

update({X,Y,N}) ->
    case {get({X,Y,N}), count_neighbours({X,Y,N})} of
        {on , 2} -> put({X, Y, N+1}, on);
        {on , 3} -> put({X, Y, N+1}, on);
        {off, 3} -> put({X, Y, N+1}, on);
        _        -> put({X, Y, N+1}, off)
    end.

init(Lines) ->
    erase(),
    lists:foldl(fun(Line, Y) ->
                        lists:foldl(fun(C, X) ->
                                            put({X, Y, 0}, onoff(C)),
                                            X+1
                                    end, 0, Line),
                        Y+1
                end, 0, Lines).

onoff($#) -> on;
onoff($.) -> off.

count_neighbours({X, Y, N}) ->
    Coords = [ {X-1, Y-1, N}, {X, Y-1, N}, {X+1, Y-1, N}
             , {X-1, Y  , N},              {X+1, Y  , N}
             , {X-1, Y+1, N}, {X, Y+1, N}, {X+1, Y+1, N}
             ],
    lists:sum(lists:map(fun on/1, Coords)).

on(Coord) ->
    case get(Coord) of
        on        -> 1;
        off       -> 0;
        undefined -> 0
    end.

input() ->
    {ok, Input} = file:read_file("input/day18"),
    string:tokens(binary_to_list(Input), "\n").

-include_lib("eunit/include/eunit.hrl").
day18_test() ->
    init([ ".#.#.#"
         , "...##."
         , "#....#"
         , "..#..."
         , "#.#..#"
         , "####.."
         ]),
    [next_state(N) || N <- lists:seq(0, 3)],
    ?assertEqual(4, count_turned_on(4)).

part1_test() ->
     ?assertEqual(814, solve_part1()).


part2_test() ->
     ?assertEqual(924, solve_part2()).

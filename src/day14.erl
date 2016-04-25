%% --- Day 14: Reindeer Olympics ---
%%
%% This year is the Reindeer Olympics! Reindeer can fly at high
%% speeds, but must rest occasionally to recover their energy. Santa
%% would like to know which of his reindeer is fastest, and so he has
%% them race.
%%
%% Reindeer can only either be flying (always at their top speed) or
%% resting (not moving at all), and always spend whole seconds in
%% either state.
%%
%% For example, suppose you have the following Reindeer:
%%
%% - Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
%% - Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
%%
%% After one second, Comet has gone 14 km, while Dancer has gone 16
%% km. After ten seconds, Comet has gone 140 km, while Dancer has gone
%% 160 km. On the eleventh second, Comet begins resting (staying at
%% 140 km), and Dancer continues on for a total distance of 176 km. On
%% the 12th second, both reindeer are resting. They continue to rest
%% until the 138th second, when Comet flies for another ten
%% seconds. On the 174th second, Dancer flies for another 11 seconds.
%%
%% In this example, after the 1000th second, both reindeer are
%% resting, and Comet is in the lead at 1120 km (poor Dancer has only
%% gotten 1056 km by that point). So, in this situation, Comet would
%% win (if the race ended at 1000 seconds).
%%
%% Given the descriptions of each reindeer (in your puzzle input),
%% after exactly 2503 seconds, what distance has the winning reindeer
%% traveled?

-module(day14).
-compile([export_all]).

solve_part1() ->
    solve(input(), 2503).

solve(Input, Time) ->
    Raindeers = parse(Input),
    lists:max([{distance(flying, R, Time), Name} || {R, Name} <- Raindeers]).

parse(Input) ->
    lists:map(fun(Line) ->
                      {match, [Name, Speed, Duration, Rest]} =
                          re:run(Line,
                                 "(\\S+) can fly (\\d+) km/s for (\\d+) seconds, "
                                 "but then must rest for (\\d+) seconds.",
                                 [{capture, all_but_first, list}]),
                      { { list_to_integer(Speed)
                        , list_to_integer(Duration)
                        , list_to_integer(Rest)
                        }
                      , Name}
                end, Input).

input() ->
    {ok, Input} = file:read_file("input/day14"),
    string:tokens(binary_to_list(Input), "\n").

distance(_, _, 0) ->
    0;
distance(flying, {Speed, Duration, Rest}, Time) ->
    Elapsed = min(Time, Duration),
    (Elapsed * Speed) + distance(resting, {Speed, Duration, Rest}, Time - Elapsed);
distance(resting, {Speed, Duration, Rest}, Time) ->
    Elapsed = min(Time, Rest),
    distance(flying, {Speed, Duration, Rest}, Time - Elapsed).


-include_lib("eunit/include/eunit.hrl").
day14_test() ->
    Input = [ "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
            , "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
            ],
    ?assertEqual({1120, "Comet"}, solve(Input, 1000)).

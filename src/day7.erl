%% --- Day 7: Some Assembly Required ---
%%
%% This year, Santa brought little Bobby Tables a set of wires and
%% bitwise logic gates! Unfortunately, little Bobby is a little under
%% the recommended age range, and he needs help assembling the
%% circuit.
%%
%% Each wire has an identifier (some lowercase letters) and can carry
%% a 16-bit signal (a number from 0 to 65535). A signal is provided to
%% each wire by a gate, another wire, or some specific value. Each
%% wire can only get a signal from one source, but can provide its
%% signal to multiple destinations. A gate provides no signal until
%% all of its inputs have a signal.
%%
%% The included instructions booklet describes how to connect the
%% parts together: x AND y -> z means to connect wires x and y to an
%% AND gate, and then connect its output to wire z.
%%
%% For example:
%%
%%   123 -> x means that the signal 123 is provided to wire x.
%%
%%   x AND y -> z means that the bitwise AND of wire x and wire y is
%%   provided to wire z.
%%
%%   p LSHIFT 2 -> q means that the value from wire p is left-shifted
%%   by 2 and then provided to wire q.
%%
%%   NOT e -> f means that the bitwise complement of the value from
%%   wire e is provided to wire f.
%%
%% Other possible gates include OR (bitwise OR) and RSHIFT
%% (right-shift). If, for some reason, you'd like to emulate the
%% circuit instead, almost all programming languages (for example, C,
%% JavaScript, or Python) provide operators for these gates.
%%
%% For example, here is a simple circuit:
%%
%%  123 -> x
%%  456 -> y
%%  x AND y -> d
%%  x OR y -> e
%%  x LSHIFT 2 -> f
%%  y RSHIFT 2 -> g
%%  NOT x -> h
%%  NOT y -> i
%%
%% After it is run, these are the signals
%% on the wires:
%%
%%  d: 72
%%  e: 507
%%  f: 492
%%  g: 114
%%  h: 65412
%%  i: 65079
%%  x: 123
%%  y: 456
%%
%% In little Bobby's kit's instructions booklet (provided as your
%% puzzle input), what signal is ultimately provided to wire a?

-module(day7).
-compile([export_all]).

solve_part1() ->
    solve(input(), #{}).

solve_part2() ->
    solve(input(), #{"b" => 956}).

input() ->
    {ok, Input} = file:read_file("input/day7"),
    string:tokens(binary_to_list(Input), "\n").

solve(Input, S) ->
    Ops = parse_ops(Input),
    eval_loop(Ops, S).

parse_ops(Input) ->
    lists:map(fun parse_op/1, Input).

parse_op(Line) ->
    case string:tokens(Line, " ") of
        [A, "->", K]              -> {K, {A,    fun(X) -> X end}};
        ["NOT", A, "->", K]       -> {K, {A,    fun(X) -> b_not(X) end}};
        [A, "AND", B, "->", K]    -> {K, {A, B, fun(X, Y) -> X band Y end}};
        [A, "OR", B, "->", K]     -> {K, {A, B, fun(X, Y) -> X bor Y end}};
        [A, "RSHIFT", B, "->", K] -> {K, {A, B, fun(X, Y) -> X bsr Y end}};
        [A, "LSHIFT", B, "->", K] -> {K, {A, B, fun(X, Y) -> X bsl Y end}}
    end.

b_not(X) ->
    %% Simulate 16 bit numbers
    (bnot X) band ((1 bsl 16)-1).

eval_loop([], S) ->
    S;
eval_loop([{K, Op}|Ops], S) ->
    case maps:is_key(K, S) of
        true ->
            eval_loop(Ops, S);
        false ->
            case eval(Op, S) of
                {done, Val} -> eval_loop(Ops, maps:put(K, Val, S));
                incomplete  -> eval_loop(Ops ++ [{K, Op}], S)
            end
    end.

eval({A, Fun}, S) ->
    case infer(A, S) of
        {ok, X} -> {done, Fun(X)};
        error   -> incomplete
    end;
eval({A, B, Fun}, S) ->
    case {infer(A, S), infer(B, S)} of
        {{ok, X}, {ok, Y}} -> {done, Fun(X, Y)};
        _                  -> incomplete
    end.

infer(X, S) ->
    case string:to_integer(X) of
        {error, _} -> maps:find(X, S);
        {Int, _}   -> {ok, Int}
    end.

-include_lib("eunit/include/eunit.hrl").

day7_test() ->
    Expected = #{ "d" => 72
                , "e" => 507
                , "f" => 492
                , "g" => 114
                , "h" => 65412
                , "i" => 65079
                , "x" => 123
                , "y" => 456},
    Input = [ "123 -> x"
            , "456 -> y"
            , "x AND y -> d"
            , "x OR y -> e"
            , "x LSHIFT 2 -> f"
            , "y RSHIFT 2 -> g"
            , "NOT x -> h"
            , "NOT y -> i"
            ],
    ?assertEqual(Expected, solve(Input, #{})).

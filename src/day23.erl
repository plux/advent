%% --- Day 23: Opening the Turing Lock ---
%%
%% Little Jane Marie just got her very first computer for Christmas
%% from some unknown benefactor. It comes with instructions and an
%% example program, but the computer itself seems to be
%% malfunctioning. She's curious what the program does, and would like
%% you to help her run it.
%%
%% The manual explains that the computer supports two registers and
%% six instructions (truly, it goes on to remind the reader, a
%% state-of-the-art technology). The registers are named a and b, can
%% hold any non-negative integer, and begin with a value of 0. The
%% instructions are as follows:
%%
%% - hlf r sets register r to half its current value, then continues
%%   with the next instruction.
%%
%% - tpl r sets register r to triple its current value, then continues
%%   with the next instruction.
%%
%% - inc r increments register r, adding 1 to it, then continues with
%%   the next instruction.
%%
%% - jmp offset is a jump; it continues with the instruction offset
%%   away relative to itself.
%%
%% - jie r, offset is like jmp, but only jumps if register r is even
%%   ("jump if even").
%%
%% - jio r, offset is like jmp, but only jumps if register r is 1
%%   ("jump if one", not odd).
%%
%% All three jump instructions work with an offset relative to that
%% instruction. The offset is always written with a prefix + or - to
%% indicate the direction of the jump (forward or backward,
%% respectively). For example, jmp +1 would simply continue with the
%% next instruction, while jmp +0 would continuously jump back to
%% itself forever.
%%
%% The program exits when it tries to run an instruction beyond the
%% ones defined.
%%
%% For example, this program sets a to 2, because the jio instruction
%% causes it to skip the tpl instruction:
%%
%%   inc a
%%   jio a, +2
%%   tpl a
%%   inc a
%%
%% What is the value in register b when the program in your puzzle
%% input is finished executing?

-module(day23).
-compile([export_all]).

solve_part1() ->
    eval(input(), #{pc => 1, "a" => 0, "b" => 0}).

solve_part2() ->
    eval(input(), #{pc => 1, "a" => 1, "b" => 0}).

input() ->
    {ok, Input} = file:read_file("input/day23"),
    string:tokens(binary_to_list(Input), "\n").

eval(Ops, #{pc := PC, "a" := A, "b" := B}) when PC > length(Ops);
                                                PC < 1 ->
    {A, B};
eval(Ops, #{pc := PC} = State) ->
    Op = lists:nth(PC, Ops),
    eval(Ops, eval_op(Op, State)).

eval_op(Op, S) ->
    case string:tokens(Op, " ") of
        ["hlf", R] -> next(S#{R := maps:get(R, S) div 2});
        ["tpl", R] -> next(S#{R := maps:get(R, S) * 3});
        ["inc", R] -> next(S#{R := maps:get(R, S) + 1});
        ["jmp", N] -> jump(S, N);
        ["jie", [R,$,], N] ->
            case maps:get([R], S) rem 2 of
                0 -> jump(S, N);
                1 -> next(S)
            end;
        ["jio", [R,$,], N] ->
            case maps:get([R], S) of
                1 -> jump(S, N);
                _ -> next(S)
            end
    end.

next(S) -> jump(S, 1).

jump(S, N) when is_list(N) -> jump(S, list_to_integer(N));
jump(#{pc := PC} = S, N)   -> S#{pc := PC + N}.

-include_lib("eunit/include/eunit.hrl").

day23_test() ->
    Ops = [ "inc a"
          , "jio a, +2"
          , "tpl a"
          , "inc a"
          ],
    ?assertEqual({2, 0}, eval(Ops, #{pc => 1, "a" => 0, "b" => 0})).

part1_test() ->
    ?assertEqual({1, 170}, solve_part1()).

part2_test() ->
    ?assertEqual({1, 247}, solve_part2()).

%% --- Day 11: Corporate Policy ---
%%
%% Santa's previous password expired, and he needs help choosing a new one.
%%
%% To help him remember his new password after the old one expires,
%% Santa has devised a method of coming up with a password based on
%% the previous one. Corporate policy dictates that passwords must be
%% exactly eight lowercase letters (for security reasons), so he finds
%% his new password by incrementing his old password string repeatedly
%% until it is valid.
%%
%% Incrementing is just like counting with numbers: xx, xy, xz, ya,
%% yb, and so on. Increase the rightmost letter one step; if it was z,
%% it wraps around to a, and repeat with the next letter to the left
%% until one doesn't wrap around.
%%
%% Unfortunately for Santa, a new Security-Elf recently started, and
%% he has imposed some additional password requirements:
%%
%% Passwords must include one increasing straight of at least three
%% letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip
%% letters; abd doesn't count.  Passwords may not contain the letters
%% i, o, or l, as these letters can be mistaken for other characters
%% and are therefore confusing.  Passwords must contain at least two
%% different, non-overlapping pairs of letters, like aa, bb, or zz.
%% For example:
%%
%% hijklmmn meets the first requirement (because it contains the
%% straight hij) but fails the second requirement requirement (because
%% it contains i and l).
%% abbceffg meets the third requirement (because it repeats bb and ff)
%% but fails the first requirement.
%% abbcegjk fails the third requirement, because it only has one
%% double letter (bb).
%% The next password after abcdefgh is abcdffaa.
%% The next password after ghijklmn is ghjaabcc, because you
%% eventually skip all the passwords that start with ghi..., since i
%% is not allowed.
%% Given Santa's current password (your puzzle input), what should his
%% next password be?

-module(day11).
-compile([export_all]).

solve_part1() ->
    solve(input()).

solve_part2() ->
    solve(solve(input())).

solve(Input) ->
    Next = incr(Input),
    case is_valid_password(Next) of
        true  -> Next;
        false -> solve(Next)
    end.

is_valid_password(Input) ->
    has_increasing_triple(Input) andalso
    no_forbidden_letter(Input) andalso
    has_pairs(Input, 2).

input() ->
    "cqjxjnds".

incr(Str) ->
    lists:reverse(incr1(lists:reverse(Str))).

incr1([])     -> [$a];
incr1([$z|T]) -> [$a|incr1(T)];
incr1([H|T])  -> [H+1|T].

has_increasing_triple([A, B, C|_]) when B =:= A+1, C =:= A+2 ->
    true;
has_increasing_triple([_|T]) ->
    has_increasing_triple(T);
has_increasing_triple(_) ->
    false.

no_forbidden_letter(Str) ->
    lists:all(fun(C) -> not lists:member(C, Str) end, "iol").

has_pairs([A,A|T], N) -> has_pairs(T, N-1);
has_pairs([_|T], N)   -> has_pairs(T, N);
has_pairs(_, 0)       -> true;
has_pairs(_, _)       -> false.


-include_lib("eunit/include/eunit.hrl").

day11_test_() ->
    [ ?_assertEqual("b",  incr("a"))
    , ?_assertEqual("aa", incr("z"))
    , ?_assertEqual("ba", incr("az"))
    , ?_assert(has_increasing_triple("fooabcggg"))
    , ?_assert(has_increasing_triple("kkxyzll"))
    , ?_assertNot(has_increasing_triple("abd"))
    , ?_assert(no_forbidden_letter("kka"))
    , ?_assertNot(no_forbidden_letter("afaiol"))
    , ?_assert(has_pairs("foobaa", 2))
    , ?_assertNot(has_pairs("foobar", 2))
    , ?_assertEqual("abcdffaa", solve("abcdefgh"))
    , ?_assertEqual("ghjaabcc", solve("ghijklmn"))
    ].

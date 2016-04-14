%% --- Day 5: Doesn't He Have Intern-Elves For This? ---
%%
%% Santa needs help figuring out which strings in his text file are
%% naughty or nice.
%%
%% A nice string is one with all of the following properties:
%%
%% It contains at least three vowels (aeiou only), like aei, xazegov,
%% or aeiouaeiouaeiou.  It contains at least one letter that appears
%% twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or
%% dd).  It does not contain the strings ab, cd, pq, or xy, even if
%% they are part of one of the other requirements.  For example:
%%
%% ugknbfddgicrmopn is nice because it has at least three vowels
%% (u...i...o...), a double letter (...dd...), and none of the
%% disallowed substrings.  aaa is nice because it has at least three
%% vowels and a double letter, even though the letters used by
%% different rules overlap.  jchzalrnumimnmhp is naughty because it
%% has no double letter.  haegwjzuvuyypxyu is naughty because it
%% contains the string xy.  dvszwmarrgswjxmb is naughty because it
%% contains only one vowel.  How many strings are nice?
%%
%% --- Part Two ---
%%
%% Realizing the error of his ways, Santa has switched to a better
%% model of determining whether a string is naughty or nice. None of
%% the old rules apply, as they are all clearly ridiculous.
%%
%% Now, a nice string is one with all of the following properties:
%%
%% It contains a pair of any two letters that appears at least twice
%% in the string without overlapping, like xyxy (xy) or aabcdefgaa
%% (aa), but not like aaa (aa, but it overlaps).  It contains at least
%% one letter which repeats with exactly one letter between them, like
%% xyx, abcdefeghi (efe), or even aaa.  For example:
%%
%% qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice
%% (qj) and a letter that repeats with exactly one letter between them
%% (zxz).  xxyxx is nice because it has a pair that appears twice and
%% a letter that repeats with one between, even though the letters
%% used by each rule overlap.  uurcxstgmygtbstg is naughty because it
%% has a pair (tg) but no repeat with a single letter between them.
%% ieodomkazucvgmuy is naughty because it has a repeating letter with
%% one between (odo), but no pair that appears twice.  How many
%% strings are nice under these new rules?


-module(day5).
-compile([export_all]).

solve_part1() ->
    solve(input(), fun is_nice/1).

solve_part2() ->
    solve(input(), fun is_nice2/1).

input() ->
    {ok, Input} = file:read_file("input/day5"),
    string:tokens(binary_to_list(Input), "\n").

solve(Input, Fun) ->
    length(lists:filter(Fun, Input)).

is_nice(Str) ->
    contains_3_vowels(Str) andalso
    has_double_letters(Str) andalso
    doesnt_contain_forbidden(Str).

is_nice2(Str) ->
    contains_pair(Str) andalso
    contains_lol(Str).

contains_pair([A,B|T]) ->
    case string:str(T, [A,B]) of
        0 -> contains_pair([B|T]);
        _ -> true
    end;
contains_pair(_) ->
    false.

contains_lol([])        -> false;
contains_lol([X,_,X|_]) -> true;
contains_lol([_|T])     -> contains_lol(T).

has_double_letters([])      -> false;
has_double_letters([X,X|_]) -> true;
has_double_letters([_|T])   -> has_double_letters(T).

contains_3_vowels(Str) ->
    length(lists:filter(fun is_vowel/1, Str)) >= 3.

is_vowel(C) ->
    lists:member(C, "aeiou").

doesnt_contain_forbidden(Str) ->
    lists:all(fun(Substr) ->
                      string:str(Str, Substr) == 0
              end, ["ab", "cd", "pq", "xy"]).

-include_lib("eunit/include/eunit.hrl").

day5_test_() ->
    [ ?_assert(is_nice("ugknbfddgicrmopn"))
    , ?_assert(contains_3_vowels("ugknbfddgicrmopn"))
    , ?_assert(has_double_letters("ugknbfddgicrmopn"))
    , ?_assert(doesnt_contain_forbidden("ugknbfddgicrmopn"))
    , ?_assert(is_nice("aaa"))
    , ?_assertNot(has_double_letters("jchzalrnumimnmhp"))
    , ?_assertNot(doesnt_contain_forbidden("haegwjzuvuyypxyu"))
    , ?_assertNot(contains_3_vowels("dvszwmarrgswjxmb"))
    , ?_assert(is_nice2("qjhvhtzxzqqjkmpb"))
    , ?_assert(is_nice2("xxyxx"))
    , ?_assertNot(is_nice2("uurcxstgmygtbstg"))
    , ?_assertNot(contains_pair("ieodomkazucvgmuy"))
    ].

%% --- Day 4: The Ideal Stocking Stuffer ---
%%
%% Santa needs help mining some AdventCoins (very similar to bitcoins)
%% to use as gifts for all the economically forward-thinking little
%% girls and boys.
%%
%% To do this, he needs to find MD5 hashes which, in hexadecimal,
%% start with at least five zeroes. The input to the MD5 hash is some
%% secret key (your puzzle input, given below) followed by a number in
%% decimal. To mine AdventCoins, you must find Santa the lowest
%% positive number (no leading zeroes: 1, 2, 3, ...) that produces
%% such a hash.
%%
%% For example:
%%
%% If your secret key is abcdef, the answer is 609043, because the MD5
%% hash of abcdef609043 starts with five zeroes (000001dbbfa...), and
%% it is the lowest such number to do so.  If your secret key is
%% pqrstuv, the lowest number it combines with to make an MD5 hash
%% starting with five zeroes is 1048970; that is, the MD5 hash of
%% pqrstuv1048970 looks like 000006136ef....
%%
%% --- Part Two ---
%%
%% Now find one that starts with six zeroes.

-module(day4).
-compile([export_all]).

solve_part1() ->
    mine(input(), 0).

solve_part2() ->
    mine2(input(), 0).

input() ->
    "iwrupvqb".

mine(Prefix, N) ->
    case md5str(Prefix, N) of
        "00000" ++ _ -> N;
        _            -> mine(Prefix, N+1)
    end.

mine2(Prefix, N) ->
    case md5str(Prefix, N) of
        "000000" ++ _ -> N;
        _            -> mine2(Prefix, N+1)
    end.

md5str(Prefix, N) ->
    <<Bin:128>> = crypto:hash(md5, [Prefix, integer_to_list(N)]),
    lists:flatten(io_lib:format("~32.16.0b", [Bin])).

-include_lib("eunit/include/eunit.hrl").

day4_test_() ->
    [ ?_assertEqual(609043, mine("abcdef", 600000))
    , ?_assertEqual(1048970, mine("pqrstuv", 1040000))
    ].

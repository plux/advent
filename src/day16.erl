%% --- Day 16: Aunt Sue ---
%%
%% Your Aunt Sue has given you a wonderful gift, and you'd like to
%% send her a thank you card. However, there's a small problem: she
%% signed it "From, Aunt Sue".
%%
%% You have 500 Aunts named "Sue".
%%
%% So, to avoid sending the card to the wrong person, you need to
%% figure out which Aunt Sue (which you conveniently number 1 to 500,
%% for sanity) gave you the gift. You open the present and, as luck
%% would have it, good ol' Aunt Sue got you a My First Crime Scene
%% Analysis Machine! Just what you wanted. Or needed, as the case may
%% be.
%%
%% The My First Crime Scene Analysis Machine (MFCSAM for short) can
%% detect a few specific compounds in a given sample, as well as how
%% many distinct kinds of those compounds there are. According to the
%% instructions, these are what the MFCSAM can detect:
%%
%% - children, by human DNA age analysis.
%% - cats. It doesn't differentiate individual breeds.
%% - Several seemingly random breeds of dog: samoyeds, pomeranians,
%%   akitas, and vizslas.
%% - goldfish. No other kinds of fish.
%% - trees, all in one group.
%% - cars, presumably by exhaust or gasoline or something.
%% - perfumes, which is handy, since many of your Aunts Sue wear a few kinds.
%%
%% In fact, many of your Aunts Sue have many of these. You put the
%% wrapping from the gift into the MFCSAM. It beeps inquisitively at
%% you a few times and then prints out a message on ticker tape:
%%
%% children: 3
%% cats: 7
%% samoyeds: 2
%% pomeranians: 3
%% akitas: 0
%% vizslas: 0
%% goldfish: 5
%% trees: 3
%% cars: 2
%% perfumes: 1
%%
%% You make a list of the things you can remember about each Aunt
%% Sue. Things missing from your list aren't zero - you simply don't
%% remember the value.
%%
%% What is the number of the Sue that got you the gift?
%%
%% --- Part Two ---
%%
%% As you're about to send the thank you note, something in the
%% MFCSAM's instructions catches your eye. Apparently, it has an
%% outdated retroencabulator, and so the output from the machine isn't
%% exact values - some of them indicate ranges.
%%
%% In particular, the cats and trees readings indicates that there are
%% greater than that many (due to the unpredictable nuclear decay of
%% cat dander and tree pollen), while the pomeranians and goldfish
%% readings indicate that there are fewer than that many (due to the
%% modial interaction of magnetoreluctance).
%%
%% What is the number of the real Aunt Sue?

-module(day16).
-compile([export_all]).

solve_part1() ->
    find_sue(parse(input())).

solve_part2() ->
    find_sue2(parse(input())).

sue() ->
    [ {"children", 3}
    , {"cats", 7}
    , {"samoyeds", 2}
    , {"pomeranians", 3}
    , {"akitas", 0}
    , {"vizslas", 0}
    , {"goldfish", 5}
    , {"trees", 3}
    , {"cars", 2}
    , {"perfumes", 1}
    ].

input() ->
    {ok, Input} = file:read_file("input/day16"),
    string:tokens(binary_to_list(Input), "\n").

parse(Input) ->
    lists:map(fun(Line) ->
                      {match, [ Num
                              , Thing1, Amount1
                              , Thing2, Amount2
                              , Thing3, Amount3]} =
                          re:run(Line,
                                 "Sue (\\d+): "
                                 "(\\S+): (\\d+), "
                                 "(\\S+): (\\d+), "
                                 "(\\S+): (\\d+)",
                                 [{capture, all_but_first, list}]),
                      { list_to_integer(Num)
                      , [ {Thing1, list_to_integer(Amount1)}
                        , {Thing2, list_to_integer(Amount2)}
                        , {Thing3, list_to_integer(Amount3)}
                        ]}
                end, Input).

find_sue(Sues) ->
    IsMatch = fun(S) -> lists:all(fun(T) -> lists:member(T, sue()) end, S) end,
    [Num || {Num, Things} <- Sues, IsMatch(Things)].

find_sue2(Sues) ->
    IsMatch = fun(S) ->
                      lists:all(fun({K, N}) ->
                                        {K, V} = lists:keyfind(K, 1, sue()),
                                        compare(K, N, V)
                                end, S)
              end,
    [Num || {Num, Things} <- Sues, IsMatch(Things)].

compare("cats", A, B)        -> A > B;
compare("trees", A, B)       -> A > B;
compare("pomeranians", A, B) -> A < B;
compare("goldfish", A, B)    -> A < B;
compare(_, A, B)             -> A =:= B.


-include_lib("eunit/include/eunit.hrl").
day16_test() ->
    [373] = solve_part1(),
    [260] = solve_part2(),
    ok.

%% --- Day 21: RPG Simulator 20XX ---
%%
%% Little Henry Case got a new video game for Christmas. It's an RPG,
%% and he's stuck on a boss. He needs to know what equipment to buy at
%% the shop. He hands you the controller.
%%
%% In this game, the player (you) and the enemy (the boss) take turns
%% attacking. The player always goes first. Each attack reduces the
%% opponent's hit points by at least 1. The first character at or
%% below 0 hit points loses.
%%
%% Damage dealt by an attacker each turn is equal to the attacker's
%% damage score minus the defender's armor score. An attacker always
%% does at least 1 damage. So, if the attacker has a damage score of
%% 8, and the defender has an armor score of 3, the defender loses 5
%% hit points. If the defender had an armor score of 300, the defender
%% would still lose 1 hit point.
%%
%% Your damage score and armor score both start at zero. They can be
%% increased by buying items in exchange for gold. You start with no
%% items and have as much gold as you need. Your total damage or armor
%% is equal to the sum of those stats from all of your items. You have
%% 100 hit points.
%%
%% Here is what the item shop is selling:
%%
%% Weapons:    Cost  Damage  Armor
%% Dagger        8     4       0
%% Shortsword   10     5       0
%% Warhammer    25     6       0
%% Longsword    40     7       0
%% Greataxe     74     8       0
%%
%% Armor:      Cost  Damage  Armor
%% Leather      13     0       1
%% Chainmail    31     0       2
%% Splintmail   53     0       3
%% Bandedmail   75     0       4
%% Platemail   102     0       5
%%
%% Rings:      Cost  Damage  Armor
%% Damage +1    25     1       0
%% Damage +2    50     2       0
%% Damage +3   100     3       0
%% Defense +1   20     0       1
%% Defense +2   40     0       2
%% Defense +3   80     0       3
%%
%% You must buy exactly one weapon; no dual-wielding. Armor is
%% optional, but you can't use more than one. You can buy 0-2 rings
%% (at most one for each hand). You must use any items you buy. The
%% shop only has one of each item, so you can't buy, for example, two
%% rings of Damage +3.
%%
%% For example, suppose you have 8 hit points, 5 damage, and 5 armor,
%% and that the boss has 12 hit points, 7 damage, and 2 armor:
%%
%% - The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
%% - The boss deals   7-5 = 2 damage; the player goes down to 6 hit points.
%% - The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
%% - The boss deals   7-5 = 2 damage; the player goes down to 4 hit points.
%% - The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
%% - The boss deals   7-5 = 2 damage; the player goes down to 2 hit points.
%% - The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.
%% - In this scenario, the player wins! (Barely.)
%%
%% You have 100 hit points. The boss's actual stats are in your puzzle
%% input. What is the least amount of gold you can spend and still win
%% the fight?
-module(day21).
-compile([export_all]).

-record(p, {name, hp, damage, armor}).

solve_part1() ->
    lists:min([Cost || {player, Cost} <- battles()]).

battles() ->
    Boss   = boss(),
    Builds = builds(),
    [{battle(player(Items), Boss), cost(Items)} || Items <- Builds].

builds() ->
    [[Weapon, Armor, R1, R2] || Weapon <- weapons(),
                                Armor <- armor(),
                                R1 <- rings(),
                                R2 <- rings(),
                                R1 =/= R2].

boss() ->
    #p{ name   = boss
      , hp     = 104
      , damage = 8
      , armor  = 1
      }.

player(Items) ->
    #p{ name   = player
      , hp     = 100
      , damage = damage(Items)
      , armor  = armor(Items)
      }.

damage(Items) ->
    lists:sum([Damage || {_, _, Damage, _} <- Items]).

armor(Items) ->
    lists:sum([Armor || {_, _, _, Armor} <- Items]).

cost(Items) ->
    lists:sum([Cost || {_, Cost, _, _} <- Items]).

battle(P1, P2) when P1#p.hp =< 0 -> P2#p.name;
battle(P1, P2) when P2#p.hp =< 0 -> P1#p.name;
battle(P1, P2)                   -> battle(attack(P1, P2), P1).

attack(P1, P2) ->
    Damage = max(1, P1#p.damage - P2#p.armor),
    P2#p{hp = P2#p.hp - Damage}.

weapons() ->
    [ {"Shortsword",   10,     5,       0}
    , {"Warhammer",    25,     6,       0}
    , {"Longsword",    40,     7,       0}
    , {"Greataxe",     74,     8,       0}
    ].

armor() ->
    [ {"Nothing",       0,     0,       0}
    , {"Leather",      13,     0,       1}
    , {"Chainmail",    31,     0,       2}
    , {"Splintmail",   53,     0,       3}
    , {"Bandedmail",   75,     0,       4}
    , {"Platemail",   102,     0,       5}
    ].

rings() ->
   [ {"Nothing",       0,     0,       0}
   , {"Nothing",       0,     0,       0}
   , {"Damage +1",    25,     1,       0}
   , {"Damage +2",    50,     2,       0}
   , {"Damage +3",   100,     3,       0}
   , {"Defense +1",   20,     0,       1}
   , {"Defense +2",   40,     0,       2}
   , {"Defense +3",   80,     0,       3}
   ].

-include_lib("eunit/include/eunit.hrl").
battle_test() ->
    player = battle( #p{name = player, hp = 8, damage = 5, armor = 5}
                   , #p{name = boss, hp = 12, damage = 7, armor = 2}).

solve_part1_test() ->
    ?assertEqual(78, solve_part1()).

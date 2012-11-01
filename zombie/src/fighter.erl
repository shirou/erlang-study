-module(fighter).
-export([summon_army/1, fighter1/0, genocide/2, silver_genocide/2, army/2]).

%% 軍隊召喚
summon_army(Swarm) ->
	pg:create(group),
	spawn(fighter, army, [group, Swarm]).


fighter1() ->
	receive
		%% 1回だけ通常攻撃
		{'Attack', Swarm} -> 
			io:format("attack!~n"),
			Swarm ! 'Attack', 
			fighter1();
		%% 1回だけ銀の弾丸攻撃
		{'Silver', Swarm} -> 
			io:format("silver attack!~n"),
			Swarm ! 'Silver',
			fighter1();
		{'Genocide', Swarm, Count} -> 
			genocide(Swarm, Count),
			fighter1();
		{'SilverGenocide', Swarm, Count} -> 
			silver_genocide(Swarm, Count),
			fighter1();
		_ ->
			fighter1()
	end.

%% 殺しまくり
genocide(_, Count) when Count == 0->
	ok;
genocide(Swarm, Count) when Count > 0->
	Swarm ! 'Attack',
	genocide(Swarm, Count-1).
%% 銀の弾丸で殺しまくり
silver_genocide(_, Count) when Count == 0->
	ok;
silver_genocide(Swarm, Count) when Count > 0->
	Swarm ! 'Silver',
	silver_genocide(Swarm, Count-1).


%% 軍隊プロセス
army(Group, Swarm) ->
	receive
		{'Charge', Count} ->
			io:format("Charge!!~n"),
			[ X ! {'Genocide', Swarm, Count} || X<- pg:members(Group) ],
			army(Group, Swarm);
		{'SilverCharge', Count} -> 
			io:format("Silver Charge!!~n"),
			[ X ! {'SilverGenocide', Swarm, Count} || X<- pg:members(Group) ],
			army(Group, Swarm);
		'Count' ->
			N = pg:members(Group),
			io:format("Count ~w ~n", [N]),
			army(Group, Swarm);
		'Summon' ->
			F = spawn(fighter, fighter1, []),
			pg:join(Group, F),
			army(Group, Swarm)
	end.

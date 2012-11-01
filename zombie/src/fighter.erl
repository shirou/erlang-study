-module(fighter).
-export([attack/1, genocide/2, silver/1, silver_genocide/2]).

%% 1回だけ通常攻撃
attack(Swarm) ->
	Swarm ! 'Attack'.

%% 殺しまくり
genocide(_, Count) when Count == 0->
	ok;
genocide(Swarm, Count) when Count > 0->
	Swarm ! 'Attack',
	genocide(Swarm, Count-1).


%% 1回だけ銀の弾丸攻撃
silver(Swarm) ->
	Swarm ! 'Silver'.

%% 殺しまくり
silver_genocide(_, Count) when Count == 0->
	ok;
silver_genocide(Swarm, Count) when Count > 0->
	Swarm ! 'Silver',
	silver_genocide(Swarm, Count-1).

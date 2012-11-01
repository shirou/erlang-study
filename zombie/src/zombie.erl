-module(zombie).
-export([summon_swarm/0, summon/2, swarm/1, zombie1/0]).


%% ゾンビの群れ召喚
summon_swarm() ->
	spawn(zombie, swarm, [[]]).


%% 単体ゾンビプロセスの動作
zombie1() ->
	receive
		'Attack' -> 
			io:format("."); %% 単に死ぬ
		_ ->
			zombie1()
	end.

%% 単体ゾンビプロセス召喚
summon(Pool, Count) when Count == 0 ->
	Pool;
summon(Pool, Count) when Count > 0 ->
	Z = spawn(zombie, zombie1, []),
	summon([Z|Pool], Count-1).

%% ゾンビの群れプロセス
swarm(Pool) ->
	receive
		'Attack' ->
			io:format("attacked~n"),
			N = length(Pool),
			if 
				N == 1 -> io:format("YOU WIN ~n");
				true ->
					[Z|T] = Pool,
					Z ! 'Attack',
					P = summon(T, 1),  %% respawn!!
					io:format("respawn~n"),
					swarm(P)
			end;
		%% 銀の弾丸だとrespawnしない
		'Silver' -> 
			io:format("attacked by Silver Bullet~n"),
			N = length(Pool),
			if 
				N == 1 -> io:format("YOU WIN ~n");
				true ->
					[Z|T] = Pool,
					Z ! 'Attack',
					swarm(T)
			end;
		'Count' ->
			io:format("Count ~w ~n", [length(Pool)]),
			swarm(Pool);
		{'Summon', Count} ->
			P = summon(Pool, Count),
			swarm(P)
	end.

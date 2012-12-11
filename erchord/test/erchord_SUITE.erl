-module(erchord_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([all/0, groups/0]).

groups() ->
	[{nodes,
	  [parallel, {repeat, 10}],
	  [node_1, node_2, node_3]}].

all() ->
	[{groups, nodes}].

init_per_group(nodes, Config) ->
    InitNode = erchord:create(),
	[{initnode,InitNode} | Config];
init_per_group(_, Config) ->
    InitNode = erchord:create(),
	[{initnode,InitNode} | Config].
%%end_per_group(nodes, _Config) ->
	%%ok.
%%end_per_group(_, _Config) ->
%%    ok.

node_1(_Config) ->
	InitNode = ?config(initnode, _Config),
	P = erchord:create(),
	P ! {join_to, InitNode},
	timer:sleep(10).

node_2(_Config) ->
	InitNode = ?config(initnode, _Config),
	P = erchord:create(),
	P ! {join_to, InitNode},
	timer:sleep(10).

node_3(_Config) ->
	InitNode = ?config(initnode, _Config),
	P = erchord:create(),
	P ! {join_to, InitNode},
	timer:sleep(10).

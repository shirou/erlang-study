-module(erchord).

-export([create/0, node/3, get_hash/1, is_inside/3]).
%% Defines
-define(StabilizationTime, 10).


%% create a new Chord node.
create() ->
	P = spawn(?MODULE, node, [false, false, dict:new()]),
	P ! {set_successor, P},
	P ! {set_predeccessor, P},
	P.
	

%% get SHA1(160bit) Hash from Node PID or Key.
get_hash(Data) ->
	crypto:sha(term_to_binary(Data)).

%% return true if Data is in range.
%% NOTE: this is not consider the chord is Ring. should consider about 0.
is_inside(Data, Start, End) when Start == End ->
    true;
is_inside(Data, Start, End) when Start < End ->
	D = get_hash(Data),
	S = get_hash(Start),
	E = get_hash(End),
	S < D andalso E >= D;
is_inside(Data, Start, End)->
	D = get_hash(Data),
	S = get_hash(Start),
	E = get_hash(End),
	S < D orelse E >= D.

node(Predeccessor, Successor, Dict) ->
    % Start stabilization whithin 10 sec
	Random_Time2Stabilization = random:uniform(1000 * ?StabilizationTime),

	receive

%%% Dict
		{put, Key, Value} -> 
			H = get_hash(Key),
			case (is_inside(Key, self(), Successor)) of
				true  ->
					%% store in this node
					node(Predeccessor, Successor, dict:store(Key, Value, Dict));
				false ->
					%% pass to the Successor
					Successor ! {put, Key, Value}, 
					node(Predeccessor, Successor, Dict)
			end;
		{get, Key} ->
			V = dict:find(Key, Dict),
			io:format("Get: ~w from ~w ~n", [V, get_hash(self())]),
			node(Predeccessor, Successor, Dict);

%%% Ring Management
		%% order send join request to RingNode
		{join_to, RingNode} -> 
			io:format("join_to(~w) at ~w ~n",[RingNode, self()]),
			RingNode ! {join, self()},
			node(Predeccessor, Successor, Dict);
		%% join request from other node
		{join, NewNode} ->
			io:format("join(~w) at ~w ~n",[NewNode, self()]),
			case (is_inside(NewNode, self(), Successor)) of
				true  ->
					%% tell the NewNode to set my Successor
					NewNode ! {set_successor, Successor},
					node(Predeccessor, Successor, Dict);
				false ->
					%% pass to the Successor
					Successor ! {join, NewNode}, 
					node(Predeccessor, Successor, Dict)
			end;
		%% set Predeccessor
		{notify, P} ->
			io:format("notify(~w) at ~w ~n",[P, self()]),
			P = is_inside(P, Predeccessor, self()),
			if
				Predeccessor == false -> 
					%% data transfer to P
					node(P, Successor, Dict);
				P == true ->
					%% data transfer to P
					node(P, Successor, Dict);
				true ->
					node(Predeccessor, Successor, Dict)
			end;
		{find_successor, SrcNode} ->
			io:format("find Successor: ~w ~n", [Successor]),
			node(Predeccessor, Successor, Dict);
		{find_predeccessor, SrcNode} ->
			io:format("find predeccessor: ~w ~n", [Successor]),
			node(Predeccessor, Successor, Dict);
		{set_successor, S} ->
			io:format("set successor(~w) at ~w ~n", [S, self()]),
			node(Predeccessor, S, Dict);
		{set_predeccessor, P} ->
			io:format("set predeccessor(~w) at ~w ~n", [P, self()]),
			node(P, Successor, Dict);
		%% initial stabilize
		{stabilize} ->
			Successor ! {stabilize, self()},
			node(Predeccessor, Successor, Dict);
		{stabilize, SrcNode} ->
			io:format("stabilize(~w) at ~w ~n",[SrcNode, self()]),
			SrcNode ! {stabilize_reply, Predeccessor},
			node(Predeccessor, Successor, Dict);
		{stabilize_reply, P} ->
			io:format("stabilize_reply(~w) at ~w ~n",[P, self()]),
			NewSuccessor = is_inside(P, self(), Successor),
			if
				%% normal
				P == self() ->
					node(Predeccessor, Successor, Dict);
				%% P is in the between self and Successor
				NewSuccessor == true ->
					P ! {notify, self()},
					node(Predeccessor, P, Dict);
				true ->
					node(Predeccessor, Successor, Dict)
			end;

%%% get node information
		{get_successor} ->
			io:format("successor(~w) at ~w ~n", [Successor, self()]),
			node(Predeccessor, Successor, Dict);
		{get_predeccessor} ->
			io:format("predeccessor(~w) at ~w ~n", [Predeccessor, self()]),
			node(Predeccessor, Successor, Dict);
		{dict} ->
			io:format("Keys: ~w in ~w ~n", [dict:fetch_keys(Dict), get_hash(self())]),
			node(Predeccessor, Successor, Dict);
		{terminate} ->
			io:format("terminate ~w ~n", [self()]);
		_ -> 
			node(Predeccessor, Successor, Dict)
%%	after Random_Time2Stabilization ->
%%			io:format("Node ~p Running Stabilization after ~p msec ~n", [self(), Random_Time2Stabilization]),
%%			Successor ! {stabilize, self()},
%%
%%			node(Predeccessor, Successor, Dict)
	end.
				
			 


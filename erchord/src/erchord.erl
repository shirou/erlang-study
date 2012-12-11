-module(erchord).

-export([create/0, loop/1, get_hash/1, is_inside/3]).
%%-compile(export_all).

%% Defines
-define(StabilizationTime, 10).
%% Records
-record(state, {predeccessor, successor, dict}).

%% create a new Chord node.
create() ->
	S = #state{predeccessor=false, successor=false, dict=dict:new()},
	P = spawn(?MODULE, loop, [S]),
	P ! {set_successor, P},
	P ! {set_predeccessor, P},
	P.
	

%% get SHA1(160bit) Hash from Loop PID or Key.
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put(Pid, MsgRef, Key, Value, S = #state{}) ->
	H = get_hash(Key),
	case (is_inside(Key, self(), S#state.successor)) of
		true  ->
			%% store in this loop
			loop(#state{dict=dict:store(Key, Value, S#state.dict)});
		false ->
			%% pass to the Successor
			S#state.successor ! {Pid, MsgRef, {put, Key, Value}}, 
			loop(S)
	end.

get(Pid, MsgRef, Key, S = #state{}) ->
	V = dict:find(Key, S#state.dict),
	io:format("Get: ~w from ~w ~n", [V, get_hash(self())]),
	loop(S).

join_to(Pid, MsgRef, RingLoop, S = #state{}) ->
	io:format("join_to(~w) at ~w ~n",[RingLoop, self()]),
	RingLoop ! {Pid, MsgRef, {join, self()}},
	loop(S).

join(Pid, MsgRef, NewLoop, S = #state{}) ->
	io:format("join(~w) at ~w ~n",[NewLoop, self()]),
	case (is_inside(NewLoop, self(), S#state.successor)) of
		true  ->
			%% tell the NewLoop to set my Successor
			NewLoop ! {self(), MsgRef, {set_successor, S#state.successor}},
			loop(S);
		false ->
			%% pass the message to the Successor
			S#state.successor ! {self(), MsgRef, {join, NewLoop}},
			loop(S)
	end.

notify(Pid, MsgRef, P, S = #state{}) ->
	io:format("notify(~w) at ~w ~n",[P, self()]),
	P2 = is_inside(P, S#state.predeccessor, self()),
	if
		S#state.predeccessor == false -> 
			%% data transfer to P
			loop(S#state{predeccessor=P2});
		P == true ->
			%% data transfer to P
			loop(S#state{predeccessor=P2});
		true ->
			loop(S)
	end.

stabilize_reply(Pid, MsgRef, P, S = #state{}) ->
	io:format("stabilize_reply(~w) at ~w ~n",[P, self()]),
	NewSuccessor = is_inside(P, self(), S#state.successor),
	if
		%% normal
		P == self() ->
			loop(S);
		%% P is in the between self and Successor
		NewSuccessor == true ->
			P ! {notify, self()},
			loop(S#state{successor=P});
		true ->
			loop(S)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop(S = #state{}) ->
    % Start stabilization whithin 10 sec
	Random_Time2Stabilization = random:uniform(1000 * ?StabilizationTime),

	receive
%%% Dict
		{Pid, MsgRef, {put, Key, Value}} -> 
			put(Pid, MsgRef, Key, Value, S);
		{Pid, MsgRef, {get, Key}} ->
			get(Pid, MsgRef, Key, S);
%%% Ring Management
		%% order send join request to RingLoop
		{Pid, MsgRef, {join_to, RingLoop}} -> 
			join_to(Pid, MsgRef, RingLoop, S);
		%% join request from other loop
		{Pid, MsgRef, {join, NewLoop}} ->
			join(Pid, MsgRef, NewLoop, S);
		%% set Predeccessor
		{Pid, MsgRef, {notify, P}} ->
			notify(Pid, MsgRef, P, S);
		{Pid, MsgRef, {find_successor, SrcLoop}} ->
			io:format("find Successor: ~w ~n", [S#state.successor]),
			loop(S);
		{Pid, MsgRef, {find_predeccessor, SrcLoop}} ->
			io:format("find predeccessor: ~w ~n", [S#state.successor]),
			loop(S);
		{Pid, MsgRef, {set_successor, NewSuccessor}} ->
			io:format("set successor(~w) at ~w ~n", [S, self()]),
			loop(S#state{successor=NewSuccessor});
		{Pid, MsgRef, {set_predeccessor, P}} ->
			io:format("set predeccessor(~w) at ~w ~n", [P, self()]),
			loop(S#state{predeccessor=P});
		%% initial stabilize
		{Pid, MsgRef, {stabilize}} ->
			S#state.successor ! {stabilize, self()},
			loop(S);
		{Pid, MsgRef, {stabilize, SrcLoop}} ->
			io:format("stabilize(~w) at ~w ~n",[SrcLoop, self()]),
			SrcLoop ! {stabilize_reply, S#state.predeccessor},
			loop(S);
		{Pid, MsgRef, {stabilize_reply, P}} ->
			stabilize_reply(Pid, MsgRef, P, S);

%%% get loop information
		{Pid, MsgRef, {get_successor}} ->
			io:format("successor(~w) at ~w ~n", [S#state.successor, self()]),
			loop(S);
		{Pid, MsgRef, {get_predeccessor}} ->
			io:format("predeccessor(~w) at ~w ~n", [S#state.predeccessor, self()]),
			loop(S);
		{Pid, MsgRef, {dict}} ->
			io:format("Keys: ~w in ~w ~n", [dict:fetch_keys(S#state.dict), get_hash(self())]),
			loop(S);
		{Pid, MsgRef, {terminate}} ->
			io:format("terminate ~w ~n", [self()]);
		Unknown -> 
			io:format("Unknown message: ~p~n",[Unknown]),
		loop(S)
%%	after Random_Time2Stabilization ->
%%			io:format("Loop ~p Running Stabilization after ~p msec ~n", [self(), Random_Time2Stabilization]),
%%			S#state.successor ! {stabilize, self()},
%%
%%			loop(S)
	end.
				
			 


-module(hg).

-compile(export_all).

start()->
	spawn(?MODULE, init, []).

init() ->
	Hg = open_port({spawn,"/usr/local/bin/hg --config ui.interactive=True serve --cmdserver pipe"},
				   [binary,
					{cd, "~/Works/erlang-study/"},
					use_stdio,
					stderr_to_stdout,
					eof,
					exit_status
				   ]),
	port_connect(Hg, self()),
	io:format("Pid: ~w~n",[erlang:port_info(Hg)]),
	loop(Hg, 5000).


loop(Hg, Timeout) ->
	receive
		{Hg, {data, Data}} ->
			io:format("Data: ~s~n", [Data]),
%%			erlang:port_call(Hg, 0, "runcommand\n7summary");
			port_command(Hg, <<"runcommand",10,"7summary\n\n\0",0>>);
%%			Hg ! {self(), {command, 'runcommand\n7summary'}},
		{Hg, eof} ->
			init();  %% restart
		{port_info} ->
			io:format("Pid: ~w~n",[erlang:port_info(Hg)]);
		{summary} ->
			%%			port_command(Hg, <<"runcommand",10,"7summary",0>>),
			Hg ! {self(), {command, 'runcommand\n7summary'}};
		Any ->
			io:format("No match fifo_client:loop/1, ~p~n",[Any])
	after Timeout ->
			throw(timeout)
	end,
	loop(Hg, Timeout).

	

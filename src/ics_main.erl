%% @author erarafo
%% @doc @todo Add description to ics_main.


-module(ics_main).

-export([start/1]).


start([DepthS, IcsPortDefaultS, ControlPortDefaultS, Log]=Args) ->
	try
		io:fwrite("args: ~p~n", [Args]),
		{ok, _} = core_logger:start(Log),
		
		%% TODO, the string below is duplicated in ics_ics
		case core_util:initVm("/home/erarafo/git/antonius/lib", ics) of
			{ok} ->
				ok;
			_ ->
				init:stop()
		end,
		
		cmd_dict:setupCommands(),
		
		Depth = list_to_integer(DepthS),
		
		IcsPortTry = list_to_integer(IcsPortDefaultS),
		IcsPortMax = IcsPortTry + 10,
		OptionsIcs = [{nodelay, true}],
		{ok, ListenIcs, IcsPort} = get_listener_socket(IcsPortTry, IcsPortMax, OptionsIcs),
		core_logger:logLine("ICS port: ~p", [IcsPort]),
		
		ListenIcsPid = spawn(ics_ics, start, [self(), ListenIcs, Depth]),   %% TODO, self() is not used

		ControlPortTry = list_to_integer(ControlPortDefaultS),
		ControlPortMax = ControlPortTry + 10,
		OptionsControl = [],
		{ok, ListenControl, ControlPort} = get_listener_socket(ControlPortTry, ControlPortMax, OptionsControl),
		core_logger:logLine("control port: ~p", [ControlPort]),
		spawn(ics_control, start, [self(), ListenControl]),
		
		receive
			stop ->
				ListenIcsPid ! stop,
				ok
		end
	catch X:Y:S ->
			  core_logger:logLine("caught: ~p, ~p, stack: ~p", [X, Y, S])
	after
		core_logger:logLine("terminating: ~p", [?MODULE]),
		ok = core_logger:stop(),
		init:stop()
	end.


get_listener_socket(Port, PortMax, _Options) when Port > PortMax ->
	nok;

get_listener_socket(Port, PortMax, Options) ->
	case gen_tcp:listen(Port, Options) of
		{error, eaddrinuse} ->
			get_listener_socket(Port+1, PortMax, Options);
		{ok, Socket} ->
			{ok, Socket, Port}
	end.


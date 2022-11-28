%% @author erarafo
%% @doc @todo Add description to ics_main.


-module(ics_main).

-export([start/1]).


start([IcsPortDefaultS, ControlPortDefaultS, Log]=Args) ->
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

		{ok, ListenIcsPid} = ics_ics:start(list_to_integer(IcsPortDefaultS)),

		ControlPortTry = list_to_integer(ControlPortDefaultS),
		ControlPortMax = ControlPortTry + 10,
		OptionsControl = [],
		{ok, ListenControl, ControlPort} = ics_lib:get_listener_socket(ControlPortTry, ControlPortMax, OptionsControl),
		core_logger:logLine("control port: ~p", [ControlPort]),
		spawn(ics_control, start, [self(), ListenControl]),
		
		receive
			stop ->
				%% received 'stop' from the ics_control process
				gen_server:cast(ListenIcsPid, stop)
		end
	catch X:Y:S ->
			  core_logger:logLine("caught: ~p, ~p, stack: ~p", [X, Y, S])
	after
		core_logger:logLine("terminating: ~p", [?MODULE]),
		ok = core_logger:stop(),
		init:stop()
	end.





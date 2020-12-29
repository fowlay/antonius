%% @author erarafo
%% @doc @todo Add description to ics_control.


-module(ics_control).

-export([start/2]).


start(Master, ListenControl) ->
	case gen_tcp:accept(ListenControl, infinity) of
		{ok, Socket} ->
			case dialog(Socket, Master, "") of
				ok ->
					start(Master, ListenControl);
				stop ->
					core_logger:logLine("terminating: ~p", [?MODULE]),
					ok
			end
	end.


dialog(Socket, Master, Acc) ->
	receive
		{tcp_closed, _Port} ->
			ok;
		{tcp, _Port, Data} when is_list(Data) ->
			NewAcc = Acc ++ Data,
			case lists:reverse(NewAcc) of
				[$\n, $\r|_] ->
					Request = string:trim(NewAcc),
					case Request of
						"quit" ->
							gen_tcp:close(Socket),
							Master ! stop,
							stop;
						Other ->
							core_logger:logLine("unexpected control request: ~p", [Other]),
							dialog(Socket, Master, "")
					end;
				_ ->
						dialog(Socket, Master, NewAcc)
			end;
		Other -> 
			core_logger:logLine("unexpected: ~p", [Other]),
			dialog(Socket, Master, "")
	end.

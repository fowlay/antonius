%% @author erarafo
%% @doc @todo Add description to ics_ics.


%%% TODO, make this a gen_server

-module(ics_ics).

-export([start/3]).


-record(state,
		{sessions = ordsets:new(),
		 socket
		 }).

start(_Master, ListenIcs, _Depth) ->
	State = #state{socket = ListenIcs},
	loop_accept(State).

loop_accept(State) ->
	Stop = 
		receive
			stop ->
				ordsets:fold(
				  fun(Pid, _Acc) ->
						  Pid ! stop
				  end,
				  ignore,
				  State#state.sessions),
				true
		after 1000 ->
			false
		end,
	if
		Stop ->
			core_logger:logLine("terminating: ~p", [?MODULE]);
		true ->
			case gen_tcp:accept(State#state.socket, 1000) of
				{error, timeout} ->
					%% core_logger:logLine("accept timeout", []),
					loop_accept(State);
				{error, closed} ->
					core_logger:logLine("in module: ~p, gen_tcp:accept/2 returned {error, closed}", [?MODULE]);
				{ok, Socket} ->

					core_logger:logLine("connection accepted: ~p", [Socket]),
					ok = inet:setopts(Socket, [{active, false}]),
					
					Session = spawn(xbi_controller, start, [["/home/erarafo/git/antonius/lib", "ics", Socket, self()]]),
					wait_for_started(),
					ok = gen_tcp:controlling_process(Socket, Session),
					
					core_logger:logLine("connection passed to xbi_controller:start/1", []),
					NewSessions = ordsets:add_element(Session, State#state.sessions),
					loop_accept(State#state{sessions = NewSessions})
			end
	end.


wait_for_started() ->
	receive
		started ->
			ok;
		_ ->
			wait_for_started()
	end.


%%% 'antonius' chess engine
%%% Copyright (C) 2013 Rabbe Fogelholm
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.

%% Author: erarafo
%% Created: Aug 19, 2012
%% Description: TODO: Add description to controller
-module(xbi_controller).

%%
%% Include files
%%

-include("antonius.hrl").
-include("exception.hrl").

%%
%% Exported Functions
%%
-export([start/1]).
-export([allowLogging/0]).
-export([log/2]).
-export([log/1]).
-export([stdout/1]).
-export([stdout/2]).

%% @doc Invoked directly from xboard-wrapper.sh when Mode==single, or from ics_session.
%% Valid values for Mode are: "xboard", "ics". The Socket is meaningful only in ics mode.
%% If mode==xboard then Socket=="null"

-spec start(list()) -> ok.

start([LibPath, Mode, Socket, Master]) ->
	
	if
		Socket =/= "null" ->
			Master ! started;
		true ->
			core_logger:start("/tmp/single.log")   %% TODO, where should this file be?
	end,
	
	%% core_logger:logLine("xbi_controller starting", []),
	
	persistent_term:put(theSocket, Socket),   %% TODO, really dirty
	
	case core_util:initVm(LibPath, list_to_atom(Mode)) of
		{ok} ->
			ok;
		_ ->
			init:stop()
	end,
	
	%% core_logger:logLine("xbi_controller about to setupCommands", []),
	
	cmd_dict:setupCommands(),
	
	%% core_logger:logLine("xbi_controller about to setupBoard", []),
	cli_game:setupBoard(),
	
	
	
	core_state:sput(loggingAllowed, false),
	core_state:sput(deferredLog, []),
	
	%% core_logger:logLine("xbi_controller about to create game state", []),
	
	core_gamestate:create(),
	
	%% core_logger:logLine("xbi_controller done create game state", []),
	
%% 	if
%% 		Socket =/= "null" ->
%% 			receive
%% 				proceed ->
%% 					ok
%% 			end;
%% 		true ->
%% 			ok
%% 	end,
	
	
	RequestsPid = spawn_link(xbi_requests, start, [self(), Socket]),
	if
		Socket =/= "null" ->
			ok = gen_tcp:controlling_process(Socket, RequestsPid),
			core_logger:logLine("socket control handed over yet again", []);
		true ->
			ok
	end,
	
	receive
		{ok, Xboardrequests} ->
			true
	end,
	core_logger:logLine("trace 1", []),
	
	try 
		
		loop(Xboardrequests, xbi_command:setup()),
		log("xbi_controller terminating")
	
	catch
		_A:#exception{type=Type, message=Message, reason=Reason}:Stack ->
			log(io_lib:format("caught: ~p, reason: ~p, message: ~p", [Type, Message, Reason])),
			log(io_lib:format("stack trace: ~p~n", [Stack]));
		A:B:Stack ->
			log(io_lib:format("caught: ~p: ~p~n", [A, B])),
			log(io_lib:format("stack trace: ~p~n", [Stack]))
	after
		init:stop()
	end.


allowLogging() ->
	core_state:sput(loggingAllowed, true),
	[log(String) || String <- core_state:sget(deferredLog)].

log(Format, Arguments) ->
	log(io_lib:format(Format, Arguments)).
	

log(String) ->
	case core_state:sget(loggingAllowed) of
		false ->
			core_state:sput(deferredLog, lists:append(core_state:sget(deferredLog), [String]));
		_ ->
			io:fwrite("# ~s~n", [String])
	end.



-spec stdout(string()) -> ok.

stdout(String) ->
	core_logger:logLine("xbi_controller, send data to xboard: ~p", [String]),
	try
		case core_state:sget(mode) of
			{ok, xboard} ->
				io:fwrite("~s~n", [String]);
			{ok, ics} ->
				Socket = persistent_term:get(theSocket),
				ok = gen_tcp:send(Socket, String ++ "\n")
		end
	catch
		A:B:C ->
			core_logger:logLine("caught: ~p, ~p, stack:~n~p", [A, B, C])
	end.


-spec stdout(string(), [term()]) -> ok.

stdout(Format, Data) ->
  stdout(lists:flatten(io_lib:format(Format, Data))).



%%
%% Local Functions
%%



loop(Xboardrequests, Dict) ->
	
	core_logger:logLine("trace 2", []),

	Xboardrequests ! {subtract, self()},
	Line =
		receive
			{line, U} ->
				U
		end,

    core_logger:logLine("trace 3", []),
	
	core_logger:logLine("subtracted: ~p", [Line]),
	
	% log("--> "++Line),
	case string:tokens(Line, " ") of
		[] ->
			log("unexpected empty line from xboard");
		[Keyword|Arguments] ->
			
			case dict:is_key(Keyword, Dict) of
				false ->
					log(" ignore: ~s", [Line]);
				true ->
					xbi_command:execute(Keyword, Arguments, Dict)
			end
	end,
			
	case Line of
		"quit" ->
			log("controller stops");
		_ ->
			loop(Xboardrequests, Dict)
	end.




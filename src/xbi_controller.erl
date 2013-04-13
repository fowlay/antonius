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

%%
%% Exported Functions
%%
-export([start/1]).
-export([allowLogging/0]).
-export([log/2]).
-export([log/1]).
-export([stdout/1]).
-export([stdout/2]).

%%
%% API Functions
%%

-spec start(list()) -> ok.

start([LibPath, Mode]) ->
	
	case core_util:initVm(LibPath, list_to_atom(Mode)) of
		{ok} ->
			ok;
		_ ->
			init:stop()
	end,
	
	cmd_dict:setupCommands(),
	cli_game:setupBoard(),
	
	core_state:sput(loggingAllowed, false),
	core_state:sput(deferredLog, []),
	
	core_gamestate:create(),
	
	spawn_link(xbi_requests, start, [self()]),
	receive
		{ok, Xboardrequests} ->
			true
	end,
	
	try loop(Xboardrequests, xbi_command:setup()) of
		_ ->
			log("Controller terminating"),
			ok
	catch
		throw: X ->
			log(io_lib:format("caught throw: ~w~n", [X])),
			log(io_lib:format("stack trace: ~p~n", [erlang:get_stacktrace()])),
			init:stop();
		
		exit: Y ->
			log(io_lib:format("caught exit: ~w~n", [Y])),
			log(io_lib:format("stack trace: ~p~n", [erlang:get_stacktrace()])),
			init:stop();
		
		error: Z ->
			log(io_lib:format("caught error: ~w~n", [Z])),
			log(io_lib:format("stack trace: ~p~n", [erlang:get_stacktrace()])),
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
  io:fwrite("~s~n", [String]).



-spec stdout(string(), [term()]) -> ok.

stdout(Format, Data) ->
  io:fwrite(Format++"~n", Data).



%%
%% Local Functions
%%



loop(Xboardrequests, Dict) ->
	
	Xboardrequests ! {subtract, self()},
	
	receive
		{line, Line} ->
			true
	end,
	
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






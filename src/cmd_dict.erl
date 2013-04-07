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
%% Created: Nov 10, 2012
%% Description: TODO: Add description to cmd_dict
-module(cmd_dict).

%%
%% Include files
%%

-include("command.hrl").

%%
%% Exported Functions
%%

-export([setupCommands/0]).
-export([mergeCommand/6]).
-export([getFunction/2]).
-export([getOneLiners/0]).
-export([getDescription/1]).



%%
%% API Functions
%%

setupCommands() ->
	cmd_do:init(),
	cmd_help:init(),
	cmd_history:init(),
	cmd_move:init(),
	cmd_play:init(),
	cmd_put:init(),
	cmd_quit:init(),
	cmd_set:init(),
	cmd_setup:init(),
	cmd_suggest:init(),
	cmd_undo:init().

mergeCommand(Name, Function, NArgs, NOptArgs, OneLiner, Description) ->
	
	C = #command{name=Name, 
				 function=Function, 
				 nArgs=NArgs, 
				 nOptArgs=NOptArgs, 
				 oneliner=OneLiner, 
				 description=Description},
	
	Commands = 
		case core_state:sget(commands) of
			null ->
				[];
			{ok, List} ->
				List
		end,
	
	NewCommands = lists:merge(fun(#command{name=X}, #command{name=Y}) -> X =< Y end, 
							  [C], 
							  Commands),
	
	core_state:sput(commands, NewCommands).
	
	









getOneLiners() ->
	{ok, Commands} = core_state:sget(commands),
	MaxNameLength = maxNameLength(Commands, 0),
	[tailPad(Name, MaxNameLength + 3, [])++OneLiner || #command{name=Name, oneliner=OneLiner} <- Commands].

getDescription(Command) ->
	#command{name=Name, description=D} = getCommand(Command),
	[Name, ""|D].

getFunction(Command, Arguments) ->
	#command{function=Function} = getCommand(Command, Arguments),
	Function.
	


%%
%% Local Functions
%%

maxNameLength([], U) ->
	U;

maxNameLength([#command{name=Name} | Tail], U) when length(Name) > U ->
	maxNameLength(Tail, length(Name));

maxNameLength([_ | Tail], U) ->
	maxNameLength(Tail, U).


tailPad(_, 0, R) ->
	lists:reverse(R);

tailPad([], N, R) ->
	tailPad([], N-1, " "++R);

tailPad([A|Z], N, R) ->
	tailPad(Z, N-1, [A|R]).
	





%% @doc Returns the command named by the given string.
%% Abbreviation is allowed as long as the match is unique.


-spec getCommand(string()) -> #command{}.

getCommand(Command) ->
	{ok, Commands} = core_state:sget(commands),
	case getCommandHelper(Command, Commands, []) of
		[] ->
			core_util:userException("not a command: ~s", [Command]);
		[C] ->
			C;
		Alternatives ->
						core_util:userException("ambiguous command: ~s, could be: ~p", [Command, Alternatives])
	end.



getCommandHelper(_, [], R) ->
	R;


getCommandHelper(Command, [#command{name=Name}=C|Tail], R) ->
	case lists:prefix(Command, Name) of
		true ->
			getCommandHelper(Command, Tail, R++[C]);
		_ ->
			getCommandHelper(Command, Tail, R)
	end.


%% @doc Returns the command that matches the given
%% (possibly abbreviated) command name. The number
%% of arguments is checked against the requirements
%% of the matched command.


-spec getCommand(string(), [string()]) -> #command{}.

getCommand(CommandName, Arguments) ->
	
	#command{nArgs=Nargs, nOptArgs=NOptArgs}=C = getCommand(CommandName),
	
	if
		length(Arguments) < Nargs ->
			core_util:userException("too few arguments");
		length(Arguments) > Nargs + NOptArgs ->
			core_util:userException("too many arguments");
		true ->
			C
	end.



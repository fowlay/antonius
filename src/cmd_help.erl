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
%% Description: TODO: Add description to cmd_help
-module(cmd_help).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([help/1]).

%%
%% Include files
%%
-include("antonius.hrl").

-include("piecetype.hrl").
-include("board.hrl").
-include("node.hrl").
-include("abresult.hrl").
-include("game.hrl").
-include("gameState.hrl").

-include("cmdresult.hrl").
-include("exception.hrl").

-include("command.hrl").

%%
%% API Functions
%%


init() ->
	cmd_dict:mergeCommand(
	  "help",
	  fun ?MODULE:help/1,
	  0,
	  1,
	  "display these help texts",
	  ["when no agument given, display a list of",
	   "available commands. A command name may be",
	   "specified, in which case a more detailed",
	   "explanation is given."]
						 ).


%% @doc Provide brief help on all commands,
%% or a description of a particular command.

-spec help([string()]) -> #cmdresult{}.

help([]) ->
	Lines = cmd_dict:getOneLiners(),
	#cmdresult{text=Lines};

help([Command]) ->
	Lines = cmd_dict:getDescription(Command),
	#cmdresult{text=Lines}.



%%
%% Local Functions
%%




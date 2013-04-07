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
%% Created: Feb 4, 2012
%% Description: TODO: Add description to abrunner
-module(core_abrunner).

%%
%% Include files
%%
-include("antonius.hrl").
-include("board.hrl").
-include("boardmap.hrl").
-include("node.hrl").

%%
%% Exported Functions
%%
-export([run/4]).

%%
%% API Functions
%%

-spec run(pid(), #node{}, smallint(), #board{}) -> ok.
		  
run(Caller, #node{toMove=white}=Node, Depth, Board) ->
	put(board, Board),
	V = core_node:abWhite(Node, Depth, ?SMALLINT_MIN_VALUE, ?SMALLINT_MAX_VALUE),
	Caller ! {ok, Node, V},
	ok;

run(Caller, #node{toMove=black}=Node, Depth, Board) ->
	put(board, Board),
	V = core_node:abBlack(Node, ?SMALLINT_MIN_VALUE, ?SMALLINT_MAX_VALUE, Depth),
	Caller ! {ok, Node, V},
	ok.
	
	

%%
%% Local Functions
%%


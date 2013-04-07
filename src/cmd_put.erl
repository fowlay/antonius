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
%% Description: TODO: Add description to cmd_put
-module(cmd_put).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([place/1]).

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

%%
%% API Functions
%%

init() ->
	cmd_dict:mergeCommand("put", fun ?MODULE:place/1, 3, 1,
	
	
"put a piece on the board",
			[
				"Synopsis:",
				"",
				"    put COLOUR PIECE SQUARE [TOMOVE]",
				"",
				"A PIECE of the given COLOUR is placed on the",
				"given square. Colour is specified as w or b.",
				"A piece is specified as k, q, r, b, n or p.",
				"It can also be specified who is next to move",
				"(w or b, where w is the default)."
				]).

%%
%% Local Functions
%%




%% @doc For tests, place a piece.
%%
%% TODO: 42, replace by macro

-spec place([string()]) -> #cmdresult{}.

place([SColour, [42, TypeLetter], SSquare]) ->
	place([SColour, [TypeLetter], SSquare, "w", true, false]);

place([SColour, SType, SSquare]) ->
	place([SColour, SType, SSquare, "w", false, false]);

place([SColour, [42, TypeLetter], SSquare, SToMove]) ->
	place([SColour, [TypeLetter], SSquare, SToMove, true, true]);

place([SColour, SType, SSquare, SToMove]) ->
	place([SColour, SType, SSquare, SToMove, false, true]);


place([SColour, SType, SSquare, SToMove, Pristine, IsToMoveSpecified]) ->
	Colour = core_colour:colour(SColour),
	#pieceType{name=Type} = core_piecetype:pieceType(SType, Colour),
	#board{dict=Dict} = get(board),
	Square = dict:fetch(SSquare, Dict),
	ToMove = core_colour:colour(SToMove),
	#node{white=White, black=Black} = core_gamestate:getCurrentNode(),
	
	NewMaterial =
		case Colour of
			white ->
				core_node:insertPiece(core_material:create(Type, Colour, Square, Pristine), White);
			black ->
				core_node:insertPiece(core_material:create(Type, Colour, Square, Pristine), Black)
		end,
	
	NewNode = 
		case Colour of
			white ->
				core_node:create(NewMaterial, Black, ToMove, null, 0, 0);
			black ->
				core_node:create(White, NewMaterial, ToMove, null, 0, 0)
		end,
	
	core_gamestate:removeCurrentNode(),
	core_gamestate:addNode(NewNode),
	if
		IsToMoveSpecified ->
			core_gamestate:setCurrentState(open);
		true ->
			ok
	end,
	#cmdresult{gsMoveMade=true}.
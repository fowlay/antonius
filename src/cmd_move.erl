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
%% Description: TODO: Add description to cmd_move
-module(cmd_move).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([moveFinalize/2]).

-export([move/1]).

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
	cmd_dict:mergeCommand("move", fun ?MODULE:move/1, 1, 2,"make a move (for white or black)",
				[
				"Synopsis:",
				"",
				"    move FROM TO [PROMOTION]",
				"",
				"Examples:",
				"",
				"    move e2 e4",
				"    move a7 a8 q     pawn promotion to queen",
				"    move a7 a8 n     pawn promotion to knight",
				"    move 0-0         castle short way",
				"    move 0-0-0       castle long way",
				"",
				"Captures need not be specially indicated. Promotion",
				"is to queen unless specially indicated."
				]).

%%
%% Local Functions
%%





-spec moveFinalize(colour(), #node{}) -> #cmdresult{}.

moveFinalize(Moving, NewNode) ->
	Message =
		case core_node:isCheckmated(NewNode) of
			true ->
				atom_to_list(Moving)++" has won by checkmate";
			false ->
				case core_node:isStalemate(NewNode) of
					true ->
						"stalemate";
					false ->
						null
				end
		end,
		#cmdresult{gsMoveMade=true, message=Message}.



%% @doc Make a move. Also determine the state right away.
%% TODO: This function could construct the statenode
%% immediately, instead of "updating".

-spec move([string()]) -> #cmdresult{}.

move(["0-0"]) ->
	Node = core_gamestate:getCurrentNode(),
	#node{toMove=ToMove} = Node,
	NewNode = core_node:expandAndSelect(Node, 7),
	doAddNodeAndSetStateToo(NewNode),
	moveFinalize(ToMove, NewNode);
	

move(["0-0-0"]) ->
	Node = core_gamestate:getCurrentNode(),
	#node{toMove=ToMove} = Node,
	NewNode = core_node:expandAndSelect(Node, 3),
	doAddNodeAndSetStateToo(NewNode),
	moveFinalize(ToMove, NewNode);

move([_Other]) ->
	core_util:userException("expected 0-0 or 0-0-0");

move([FromName, ToName]) ->
	move([FromName, ToName, "q"]);

move([FromName, ToName, PromotionCode]) ->
	Node = core_gamestate:getCurrentNode(),
	#node{toMove=ToMove} = Node,
	NewNode = core_node:expandAndSelect(
				     Node,
				     getSquare(FromName),
				     getSquare(ToName),
				     getPromotionType(PromotionCode)),
	doAddNodeAndSetStateToo(NewNode),
	moveFinalize(ToMove, NewNode).



%% @doc .. eliminate this one?

-spec doAddNodeAndSetStateToo(#node{}) -> ok.

doAddNodeAndSetStateToo(#node{toMove=ToMove}=Node) ->
	core_gamestate:addNode(Node),
 case core_node:isStalemate(Node) of
true ->
core_gamestate:setCurrentState(draw);
false ->
case core_node:isCheckmated(Node) of
	true ->
		case ToMove of
			white ->
				core_gamestate:setCurrentState(black_win);
			black ->
				core_gamestate:setCurrentState(white_win)
		end;
	false ->
		core_gamestate:setCurrentState(open)
end
	end.

	
	
	
	
%% 			
%% 		protected static Result addNode(Node newNode, GameState gs, Frame.Rect rectangle) {
%% 	
%% 		if (newNode.isStalemate()) {
%% 			gs.addNode(newNode, GameState.State.DRAW);
%% 			return new Command.Result(true, rectangle, true, false, false, "stalemate");
%% 		} else if (newNode.isCheckmated() && newNode.toMove == Colour.WHITE) {
%% 			gs.addNode(newNode, GameState.State.BLACK_WIN);
%% 			return new Command.Result(true, rectangle, true, false,
%% 					false, "black has won by checkmate");
%% 		} else if (newNode.isCheckmated()) {
%% 			gs.addNode(newNode, GameState.State.WHITE_WIN);
%% 			return new Command.Result(true, rectangle, true, false,
%% 					false, "white has won by checkmate");
%% 		} else {
%% 			gs.addNode(newNode, GameState.State.OPEN);
%% 			return new Command.Result(true, rectangle, true, false, false, null);
%% 		}
%% 	}	  
	
	
	
	
%% 		private Node willCauseRepetition(GameState gs, AlphaBetaResult result, Node currentNode) {
%% 		for (Iterator<AlphaBetaNode> iter = result.iterator(currentNode.toMove); iter.hasNext();) {
%% 			final AlphaBetaNode abNode = iter.next();
%% 			final String key = abNode.node.key();
%% 			if (gs.getRepeatCount(key) >= 2) {
%% 				return abNode.node;
%% 			}
%% 		}
%% 		return null;
%% 	}



%% 	private boolean drawDesired(Node currentNode, Node newNode) {
%% 		return currentNode.toMove == Colour.BLACK && newNode.blackAdvantage < 0 || 
%% 				currentNode.toMove == Colour.WHITE && newNode.blackAdvantage > 0;
%% 	}


%% 
%% %return new Command.Result(true, rectangle, true, false, true, "claiming draw by repetition after move was made");
%% 		
%% 
%% 
%% 	end,
%% 	
%% 	%% old code follows
%% 	
%% 	
%% 	
%% 	case abresult:isEmpty(Result) of
%% 		true ->
%% 			display(CurrentNode, Result),  % TODO, move in front of case .. end
%% 			case node:isCheckmated(CurrentNode) of
%% 				true ->
%% 					case CurrentNode of
%% 						#node{toMove=white} ->
%% 							game_state:setCurrentState(black_win),
%% 							cmdresult:create(true, false, false, false, "black has won by checkmate");
%% 						#node{toMove=black} ->
%% 							game_state:setCurrentState(white_win),
%% 							cmdresult:create(true, false, false, false, "white has won by checkmate")
%% 					end;
%% 				false ->
%% 					case node:isStalemate(CurrentNode) of
%% 						true ->
%% 							game_state:setCurrentState(draw),
%% 							cmdresult:create(true, false, false, false, "stalemate");
%% 						false ->
%% 							util:throw("impossible state: no move possible, yet not stalemate?", [])  % TODO static analysis
%% 					end
%% 			end;
%% 		
%% 		false ->
%% 			display(CurrentNode, Result),
%% 			[{_ABValue, NewNode}] = 
%% 				case CurrentNode of
%% 					#node{toMove=black} ->
%% 						abresult:getNHighest(1, Result);
%% 					#node{toMove=white} ->
%% 						abresult:getNLowest(1, Result)
%% 				end,
%% 
%% 			game_state:addNode(NewNode),
%% 			
%% 			case node:isCheckmated(NewNode) of
%% 				true ->
%% 					case NewNode of
%% 						#node{toMove=white} ->
%% 							game_state:setCurrentState(black_win),
%% 							cmdresult:create(true, true, false, false, "black has won by checkmate");
%% 						#node{toMove=black} ->
%% 							game_state:setCurrentState(white_win),
%% 							cmdresult:create(true, true, false, false, "white has won by checkmate")
%% 					end;
%% 				false ->
%% 					case node:isStalemate(NewNode) of
%% 						true ->
%% 							game_state:setCurrentState(draw),
%% 							cmdresult:create(true, true, false, false, "stalemate");
%% 						false ->
%% 							game_state:setCurrentState(open),
%% 							cmdresult:create(true, true, false, false, null)
%% 					end
%% 			end
%% 	end.
%% 	
	


-spec getSquare(string()) -> #square{}.

getSquare(SquareName) ->
	#board{dict=Dict} = get(board),
	case dict:is_key(SquareName, Dict) of
		false ->
			core_util:userException("not a square: ~p", [SquareName]);
		_ ->
			dict:fetch(SquareName, Dict)
	end.


-spec getPromotionType(string()) -> atom().

getPromotionType("q") -> queen;
getPromotionType("Q") -> queen;
getPromotionType("r") -> rook;
getPromotionType("R") -> rook;
getPromotionType("b") -> bishop;
getPromotionType("B") -> bishop;
getPromotionType("n") -> knight;
getPromotionType("N") -> knight;

getPromotionType(X) ->
	core_util:userException("illegal promotion type: ~p", [X]).
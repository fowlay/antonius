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
%% Created: Feb 1, 2012
%% Description: TODO: Add description to move
-module(core_move).

%%
%% Include files
%%

-include("colour.hrl").
-include("antonius.hrl").
-include("piecetype.hrl").
-include("board.hrl").
-include("move.hrl").
-include("gameState.hrl").

%%
%% Exported Functions
%%
-export([create/2]).
-export([toString/1]).
-export([toAlgebraic/1]).
-export([describeBrief/1]).
-export([describe/3]).

%%
%% API Functions
%%


-spec create(#node{}, #node{}) -> #move{}.

create(FromNode, ToNode) ->
	#move{fromNode=FromNode,
		  toNode=ToNode,
		  isCastlingShort=getCastling(FromNode, ToNode, 7),
		  isCastlingLong=getCastling(FromNode, ToNode, 3),
		  from=getFrom(FromNode, ToNode),
		  to=getTo(FromNode, ToNode),
		  promotionType=getPromotionType(FromNode, ToNode)
		  }.


-spec toString(#move{}) -> string().

toString(#move{from=From,
			   to=To,
			   isCastlingShort=CS,
			   isCastlingLong=CL,
			   fromNode=#node{toMove=ToMove, white=FW, black=FB},
			   promotionType=PT
			  }
		)->
	if
		CS =:= true ->
			"0-0";
		CL =:= true ->
			"0-0-0";
		true ->
			Material =  if ToMove =:= white -> FW; true -> FB end,
			FromPiece = core_material:lookup(Material, From),
			if
				FromPiece =:= null ->
					core_util:inconsistencyException("impossible, piece does not exist in given square");
				true ->
					ok
			end,
			#piece{type=FromPieceType} = FromPiece,
			#pieceType{rname=RName} = core_piecetype:pieceType(FromPieceType),
			#square{name=FromSquareName} = From,
			#square{name=ToSquareName} = To,
			R1 = RName ++ " from " ++ FromSquareName ++ " to " ++ ToSquareName,
			if 
				PT =:= null ->
					R1;
				true ->
					#pieceType{name=PTName} = PT,
					R1++", promotion to "++atom_to_list(PTName)
			end
	end.


toAlgebraic(#move{from=From,
			   to=To,
			   isCastlingShort=CS,
			   isCastlingLong=CL,
			   fromNode=#node{toMove=ToMove, white=FW, black=FB},
			   promotionType=PT
			  }
		)->
	if
		CS =:= true ->
			case ToMove of
				white ->
					"e1g1";
				_ ->
					"e8g8"
			end;
		CL =:= true ->
			case ToMove of
				white ->
					"e1c1";
				_ ->
					"e8c8"
			end;
		true ->
			Material =  if ToMove =:= white -> FW; true -> FB end,
			FromPiece = core_material:lookup(Material, From),
			if
				FromPiece =:= null ->
					core_util:inconsistencyException("impossible, piece does not exist in given square");
				true ->
					ok
			end,
			#square{name=FromSquareName} = From,
			#square{name=ToSquareName} = To,
			R1 = FromSquareName ++ ToSquareName,
			if 
				PT =:= null ->
					R1;
				true ->
					#pieceType{name=PTName} = PT,
					R1++string:substr(atom_to_list(PTName), 1, 1)
			end
	end.


%% Describe this move; consider it as a possible future move
%% (not yet manifest in the game state) or a made move.
			
describe(Sid, #move{fromNode=FromNode, toNode=ToNode}=M, Future) ->
	#node{toMove=FromNodeToMove} = FromNode,
	Player = FromNodeToMove,
	if
		Future ->
			Buffer = atom_to_list(Player)++" may move: "++toString(M)++repetitionStatus(Sid, M, Future);
		true ->
			Buffer = atom_to_list(Player)++" moved: "++toString(M)++repetitionStatus(Sid, M, Future)
	end,
	
	OtherPlayer = core_colour:otherColour(FromNodeToMove),
	
	Buffer2 = 
		case core_node:loss(FromNode, OtherPlayer, ToNode) of
			#piece{type=Type, square=#square{name=SquareName}} ->
				#pieceType{rname=RName} = core_piecetype:pieceType(Type),
				lists:append(Buffer,
							 if
								 Future ->
									 ", "++atom_to_list(OtherPlayer)++" will lose "++RName++" at "++SquareName;
								 true ->
									 ", "++atom_to_list(OtherPlayer)++" lost "++RName++" at "++SquareName
							 end);
			_ ->
				Buffer
		end,
	
	case core_node:isChecked(ToNode) of
		true ->
			Buffer3 = Buffer2++" - CHECK!";
		_ ->
			Buffer3 = Buffer2
	end,
	
	Buffer3.


repetitionStatus(Sid, #move{toNode=ToNode}, Future) ->
	

	Key = core_node:key(ToNode),
	
	Count = core_gamestate:getRepeatCount(Sid, Key),
	
	if
		Future, Count > 0 ->
			" (repeat count would be: "++integer_to_list(Count+1)++")";
		Future =:= false, Count > 1 ->
			" (repeat count: "++integer_to_list(Count)++")";
		true ->
			""
	end.


			
			



%% @doc Create arguments to the "move" command, for use by the
%% "do" command.
%%
%% TODO: Consider folding the constants "q", "r", "b", "n"
%% into the pieceType object.
	
-spec describeBrief(#move{}) -> [string()].
		  
describeBrief(#move{isCastlingShort=true}) ->
	["0-0"];

describeBrief(#move{isCastlingLong=true}) ->
	["0-0-0"];

describeBrief(#move{promotionType=null, from=#square{name=From}, to=#square{name=To}}) ->
	[From, To];

describeBrief(#move{promotionType=#pieceType{name=queen}, from=#square{name=From}, to=#square{name=To}}) ->
	[From, To, "q"];

describeBrief(#move{promotionType=#pieceType{name=rook}, from=#square{name=From}, to=#square{name=To}}) ->
	[From, To, "r"];

describeBrief(#move{promotionType=#pieceType{name=bishop}, from=#square{name=From}, to=#square{name=To}}) ->
	[From, To, "b"];

describeBrief(#move{promotionType=#pieceType{name=knight}, from=#square{name=From}, to=#square{name=To}}) ->
	[From, To, "n"].


%% 	public List<String> describeBrief() {
%% 		
%% 		if (isCastlingLong) {
%% 			return Game.list1("0-0-0");
%% 		} else if (isCastlingShort) {
%% 			return Game.list1("0-0");
%% 		} else {
%% 			final String promString = Move.promotionCode(fromNode, toNode);
%% 			if (promString != null) {
%% 				return Game.list3(from.name, to.name, promString);
%% 			} else {
%% 				return Game.list2(from.name, to.name);
%% 			}
%% 		}
%% 	}
	

%%
%% Local Functions
%%

-spec getFrom(#node{}, #node{}) -> #square{}.

getFrom(#node{toMove=MovingColour, pieces=FromBoardMap}, #node{pieces=ToBoardMap}) ->
	#board{tuple=BoardTuple} = get(board),
	getFromHelper(1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour).
	

-spec getFromHelper(integer(), tuple(), #boardMap{}, #boardMap{}, colour()) -> #square{}.

getFromHelper(65, _, _, _, _) ->
	core_util:inconsistencyException("'from' square not found, impossible");

getFromHelper(J, BoardTuple, FromBoardMap, ToBoardMap, MovingColour) ->
	S = element(J, BoardTuple),
	case core_boardmap:get(S, FromBoardMap) of
		null ->
			getFromHelper(J+1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour);
		#piece{colour=MovingColour} ->
			case core_boardmap:get(S, ToBoardMap) of
				null ->
					S;
				_ ->
					getFromHelper(J+1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour)
			end;
		_ ->
			getFromHelper(J+1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour)
	end.



-spec getTo(#node{}, #node{}) -> #square{}.

getTo(#node{toMove=MovingColour, pieces=FromBoardMap}, #node{pieces=ToBoardMap}) ->
	#board{tuple=BoardTuple} = get(board),
	getToHelper(1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour).


-spec getToHelper(integer(), tuple(), #boardMap{}, #boardMap{}, colour()) -> #square{}.

getToHelper(65, _, _, _, _) ->
	core_util:inconsistencyException("'to' square not found, impossible");

getToHelper(J, BoardTuple, FromBoardMap, ToBoardMap, MovingColour) ->
	S = element(J, BoardTuple),
	case core_boardmap:get(S, FromBoardMap) of
		#piece{colour=MovingColour} ->
			getToHelper(J+1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour);
		_ ->
			case core_boardmap:get(S, ToBoardMap) of
				#piece{colour=MovingColour} ->
					S;
				_ ->
					getToHelper(J+1, BoardTuple, FromBoardMap, ToBoardMap, MovingColour)
			end
	end.


-spec getCastling(#node{}, #node{}, integer()) -> boolean().

getCastling(#node{white=FW, black=FB, toMove=MovingColour},
			#node{white=TW, black=TB},
			KingDestFile) ->
	
	Rank =  if MovingColour =:= white -> 1; true -> 8 end,
	MPre =  if MovingColour =:= white -> FW; true -> FB end,
	KingPre = core_material:getKing(MPre),
	MPost =  if MovingColour =:= white -> TW; true -> TB end,
	KingPost = core_material:getKing(MPost),
	
	
	#piece{square=#square{y=YPre, x=XPre}} = KingPre,
	if
		YPre =:= Rank, XPre =:= 5 -> 
			#piece{square=#square{y=YPost, x=XPost}} = KingPost,
			if
				YPost =:= Rank, XPost =:= KingDestFile ->
					true;
				true ->
					false	
			end;
		true ->
			false
	end.



-spec getPromotionType(#node{}, #node{}) -> xpieceType().

getPromotionType(#node{toMove=MovingColour}=FromNode, ToNode) ->
	
	getH([queen, rook, bishop, knight], MovingColour, FromNode, ToNode).


getH([], _, _, _) ->
	null;

getH([PieceTypeName|Tail], MovingColour, FromNode, ToNode) ->
	PreMoveCount = count(PieceTypeName, FromNode, MovingColour),
	PostMoveCount = count(PieceTypeName, ToNode, MovingColour),
	if 
		PostMoveCount > PreMoveCount ->
			core_piecetype:pieceType(PieceTypeName);
		true ->
			getH(Tail, MovingColour, FromNode, ToNode)
	end.


count(PieceType, #node{white=White, black=Black}, Colour) ->
  Material = (if Colour =:= white -> White; true -> Black end),
  countH(Material, 0, PieceType).

countH([], R, _) ->
	R;

countH([#piece{type=PieceType}|Tail], R, PieceType) ->
	countH(Tail, R+1, PieceType);

countH([_|Tail], R, PieceType) ->
	countH(Tail, R, PieceType).
	





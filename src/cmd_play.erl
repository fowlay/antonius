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
%% Description: TODO: Add description to cmd_play
-module(cmd_play).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([init/0]).

-export([play/1]).

-export([setSuggestion/2]).

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
	cmd_dict:mergeCommand("play", fun ?MODULE:play/1, 0, 1,
						  "compute a move for black and perform it",
						  
				[
				"Synopsis:",
				"",
				"    play [DEPTH]",
				"",
				"Try to find a move and make the move. The",
				"recursion depth may be specified (it defaults",
				"to "++integer_to_list(param_parameter:getRecursionDepth())++")."
				]).
			

%%
%% Local Functions
%%


	


isRepetition() ->
	case core_gamestate:getRepetitions() of
		[] ->
			false;
		_ ->
			true
	end.



-spec play([string()]) -> #cmdresult{}.

play([]) ->
	playAtDepth(param_parameter:getRecursionDepth());

play([X]) ->
	RecursionDepth = list_to_integer(X),
	playAtDepth(RecursionDepth).


-spec playAtDepth(smallint()) -> #cmdresult{}.
	
playAtDepth(RecursionDepth) ->
	
	core_gamestate:checkGameOpen(),
	
	#node{toMove=ToMove}=CurrentNode = core_gamestate:getCurrentNode(),
	#abResult{nodeSets=NS}=Result = core_node:alphaBetaRoot(CurrentNode, RecursionDepth),
	
	Text = core_abresult:bestMoves(Result, CurrentNode),
	Iter = core_abresult:iterator(NS, ToMove),
	case core_abresult:hasNext(Iter) of
		true ->
			NewNode = getNonStalemateNode(Iter, CurrentNode),
			case isRepetition() of
				true ->
					case drawDesired(CurrentNode, NewNode) of
						true ->
							core_abresult:done(Iter),
							core_gamestate:setCurrentState(draw),
							#cmdresult{canClaimDraw=true, message="claiming draw by repetition", text=Text};
						false ->
							case willCauseRepetition(Result, CurrentNode) of
								null ->
									core_abresult:done(Iter),
									addNode(NewNode, Text);
								RepNode ->
									if
										NewNode =/= RepNode ->
											core_abresult:done(Iter),
											addNode(NewNode, Text);
										true ->
											case core_abresult:hasNext(Iter) of
												true ->
													{_AltScore, AltNode} = core_abresult:next(Iter),
													core_abresult:done(Iter),
													addNode(AltNode, Text);
												false ->
													core_abresult:done(Iter),
													addNode(NewNode, Text)
											end
									end
							end
					end;
				false ->
					case canCauseRepetition(Result, CurrentNode) of
						true ->
							RepNode = willCauseRepetition(Result, CurrentNode),
							case drawDesired(CurrentNode, NewNode) of
								true ->
									core_abresult:done(Iter),
									core_gamestate:addNode(RepNode, draw),
									#cmdresult{gsMoveMade=true, canClaimDraw=true, message="claiming draw by repetition after move was made", text=Text};
								false ->
									if
										NewNode =/= RepNode ->
											core_abresult:done(Iter),
											addNode(NewNode, Text);
										true ->
											case core_abresult:hasNext(Iter) of
												true ->
													{_AltScore, AltNode} = core_abresult:next(Iter),
													core_abresult:done(Iter),
													addNode(AltNode, Text);
												false ->
													core_abresult:done(Iter),
													addNode(NewNode, Text)
											end
									end
							end;
						false ->
							core_abresult:done(Iter),
							addNode(NewNode, Text)
					end
			end;
		false ->
			core_abresult:done(Iter),
			case core_node:isCheckmated(CurrentNode) of
				true ->
					case CurrentNode of
						#node{toMove=white} ->
							core_gamestate:setCurrentState(black_win),
							#cmdresult{gsMoveMade=true, message="black has won by checkmate", text=Text};
						#node{toMove=black} ->
							core_gamestate:setCurrentState(white_win),
							#cmdresult{gsMoveMade=true, message="white has won by checkmate", text=Text}
					end;
				false ->
					case core_node:isStalemate(CurrentNode) of
						true ->
							core_gamestate:setCurrentState(draw),
							#cmdresult{gsMoveMade=true, message="stalemate", text=Text};
						false ->
							core_gamestate:setCurrentState(open),
							#cmdresult{gsMoveMade=true, text=Text}
					end
			end
	end.



drawDesired(#node{toMove=black}, #node{blackAdvantage=BA}) when BA < 0 ->
	true;

drawDesired(#node{toMove=white}, #node{blackAdvantage=BA}) when BA > 0 ->
	true;

drawDesired(_, _) ->
	false.


%% @doc The given iterator is trusted to be non-empty. A node is
%% returned. If the player represented by the given CurrentNode
%% is in a favorable position then stalemate nodes are avoided.
%%
%% The iterator is not terminated; it is the responsibility of the
%% caller to terminate it.

-spec getNonStalemateNode(pid(), #node{}) -> #node{}.

getNonStalemateNode(Iter, CurrentNode) ->
	case core_abresult:next(Iter) of
		error ->
			core_util:inconsistencyException("cannot happen in getNonStalemateNode/2");
		{_NewScore, NewNode} ->
			case drawDesired(CurrentNode, NewNode) of
				true ->
					NewNode;
				false ->
					case core_node:isStalemate(NewNode) of
						false ->
							NewNode;
						true ->
							case core_abresult:hasNext(Iter) of
								false ->
									NewNode;
								true ->
									getNonStalemateNode(Iter, CurrentNode)
							end
					end
			end
	end.
	






-spec canCauseRepetition(#abResult{}, #node{}) -> boolean().

canCauseRepetition(Result, CurrentNode) ->
	case willCauseRepetition(Result, CurrentNode) of
		null ->
			false;
		_ ->
			true
	end.


-spec willCauseRepetition(#abResult{}, #node{}) -> #node{} | null.

willCauseRepetition(#abResult{nodeSets=L}, #node{toMove=ToMove}) ->
	Iter = core_abresult:iterator(L, ToMove),
	wcrHelper(Iter).


-spec wcrHelper(pid()) -> #node{} | null.

wcrHelper(Iter) ->
	case core_abresult:hasNext(Iter) of
		false ->
			core_abresult:done(Iter),
			null;
		true ->
			{_Score, Node} = core_abresult:next(Iter),
			Key = core_node:key(Node),
			RepeatCount = core_gamestate:getRepeatCount(Key),
			if
				RepeatCount >= 2 ->
					core_abresult:done(Iter),
					Node;
				true ->
					wcrHelper(Iter)
			end
	end.




%% @doc Performs a game_state:addNode/2 and returns
%% a #cmdresult{}.

-spec addNode(#node{}, [string()]) -> #cmdresult{}.

addNode(Node, Text) ->
	case core_node:isStalemate(Node) of
		true ->
			core_gamestate:addNode(Node, draw),
			#cmdresult{gsMoveMade=true, message="stalemate", text=Text};
		false ->
			case core_node:isCheckmated(Node) of
				true ->
					case Node#node.toMove of
						white ->
							core_gamestate:addNode(Node, black_win),
							#cmdresult{gsMoveMade=true, message="black has won by checkmate", text=Text};
						black ->
							core_gamestate:addNode(Node, white_win),
							#cmdresult{gsMoveMade=true, message="white has won by checkmate", text=Text}
					end;
				false ->
					core_gamestate:addNode(Node, open),
					#cmdresult{gsMoveMade=true, text=Text}
			end
	end.
	


%stopwatch(reset) ->
%	core_state:sput(stopwatch, erlang:now());
%
%stopwatch(read) ->
%	{ok, Timestamp} = core_state:sget(stopwatch),
%	Millis = timer:now_diff(erlang:now(), Timestamp) div 1000,
%	consoleSay("command took (ms): ~w", [Millis]).




%% @doc Store a suggestion.
%% @spec setSuggestion(integer(), [string()]) -> ok

setSuggestion(Stamp, ArgList) ->
	core_state:sput(suggestion, {Stamp, ArgList}).
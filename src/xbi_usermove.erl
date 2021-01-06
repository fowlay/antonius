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
%% Created: Aug 22, 2012
%% Description: TODO: Add description to xboardcommand_usermove
-module(xbi_usermove).

%%
%% Include files
%%

-include("node.hrl").
-include("cmdresult.hrl").

%%
%% Exported Functions
%%
-export([execute/1]).
-export([engineMove/1]). % needed by xbi_go

%%
%% API Functions
%%

-spec execute([sid()|string()]) -> ok.

execute([Sid, Move]) ->
	xbi_controller:log(Sid, "execute: usermove ~s", [Move]),
	
	% TODO, check if we are in a state where no move is possible?
	
	% TODO, add code to perform the move
	
	Arguments = translate(Sid, Move, core_gamestate:getCurrentNode(Sid)),
	Function = cmd_dict:getFunction("move", Arguments),
	apply(Function, [[Sid]++Arguments]),
	
	% unless a throw occurred we have a new game state.
	
	engineMove(Sid).


-spec engineMove(sid()) -> ok.

engineMove(Sid) ->
	case core_gamestate:getState(Sid) of
		white_win ->
			xbi_controller:stdout(Sid, "result 1-0 {White mates}");
		black_win ->
			xbi_controller:stdout(Sid, "result 1-0 {Black mates}");
		draw ->
			xbi_controller:stdout(Sid, "result 1/2-1/2 {Stalemate}");
		open ->
			
			Arguments = [integer_to_list(param_parameter:getRecursionDepth(Sid))],
			Function = cmd_dict:getFunction("play", Arguments),
			
			xbi_controller:log(Sid, "using depth: ~s", Arguments),
			
			case apply(Function, [[Sid]++Arguments]) of
				#cmdresult{gsMoveMade=false, canClaimDraw=CanClaimDraw} ->
					case core_gamestate:getState(Sid) of
						white_win ->
							xbi_controller:stdout(Sid, "result 1-0 {White mates}");
						black_win ->
							xbi_controller:stdout(Sid, "result 0-1 {Black mates}");
						draw ->
							case CanClaimDraw of
								true ->
									xbi_controller:stdout(Sid, "result 1/1-1/2 {Draw by repetition}");
								false ->
									xbi_controller:stdout(Sid, "result 1/1-1/2 {Stalemate}")
							end
					end;
				#cmdresult{gsMoveMade=true, canClaimDraw=CanClaimDraw} ->
					Current = core_gamestate:getCurrentNode(Sid),
					Preceding = core_gamestate:getPrecedingNode(Sid),
					
					timer:sleep(1800),    %% TODO, something better

					case CanClaimDraw of
						true ->
							% tested by an Xboard game
							xbi_controller:stdout(Sid, "offer draw");
						false ->
							ok
					end,
					
					xbi_controller:stdout(Sid, "move " ++ core_move:toAlgebraic(core_move:create(Preceding, Current))),
				
					xbi_controller:log(Sid, "black advantage: ~w", [Current#node.blackAdvantage]),
					
					case core_gamestate:getState(Sid) of
						white_win ->
							xbi_controller:stdout(Sid, "result 1-0 {White mates}");
						black_win ->
							xbi_controller:stdout(Sid, "result 0-1 {Black mates}");
						draw ->
							case CanClaimDraw of
								true ->
									xbi_controller:stdout(Sid, "result 1/1-1/2 {Draw by repetition}");
								false ->
									xbi_controller:stdout(Sid, "result 1/1-1/2 {Stalemate}")
							end;
						open ->
							ok
					end
			end,
			ok
	end.


%%
%% Local Functions
%%

translate(Sid, Alg, #node{pieces=BoardMap, toMove=ToMove}) ->
	FromSquareName = string:substr(Alg, 1, 2),
	ToSquareName = string:substr(Alg, 3, 2),
	FromSquare = core_board:getSquare(Sid, FromSquareName),
	#piece{type=Type} = core_boardmap:get(FromSquare, BoardMap),

	case Type of
		king ->
			case ToMove of
				white ->
					case FromSquareName of
						"e1" ->
							case ToSquareName of
								"g1" ->
									["0-0"];
								"c1" ->
									["0-0-0"];
								_ ->
									default(FromSquareName, ToSquareName, Alg)
							end;
						_ ->
							default(FromSquareName, ToSquareName, Alg)
					end;
				_ ->
					case FromSquareName of
						"e8" ->
							case ToSquareName of
								"g8" ->
									["0-0"];
								"c8" ->
									["0-0-0"];
								_ ->
									default(FromSquareName, ToSquareName, Alg)
							end;
						_ ->
							default(FromSquareName, ToSquareName, Alg)
					end
			end;
		_ ->
			default(FromSquareName, ToSquareName, Alg)
	end.


default(F, T, A) when length(A) =:= 5 ->
	[F, T, string:substr(A, 5, 1)];

default(F, T, _) ->
	[F, T].
	
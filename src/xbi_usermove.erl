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
-export([engineMove/0]).

%%
%% API Functions
%%

-spec execute([string()]) -> ok.

execute([Move]) ->
	xbi_controller:log("execute: usermove ~s", [Move]),
	
	% TODO, check if we are in a state where no move is possible?
	
	% TODO, add code to perform the move
	
	Arguments = translate(Move, core_gamestate:getCurrentNode()),
	Function = cmd_dict:getFunction("move", Arguments),
	apply(Function, [Arguments]),
	
	% unless a throw occurred we have a new game state.
	
	engineMove().


-spec engineMove() -> ok.

engineMove() ->
	case core_gamestate:getState() of
		white_win ->
			xbi_controller:stdout("result 1-0 {White mates}");
		black_win ->
			xbi_controller:stdout("result 1-0 {Black mates}");
		draw ->
			xbi_controller:stdout("result 1/2-1/2 {Stalemate}");
		open ->
			
			Arguments = [integer_to_list(param_parameter:getRecursionDepth())],
			Function = cmd_dict:getFunction("play", Arguments),
			
			xbi_controller:log("using depth: ~s", Arguments),
			
			case apply(Function, [Arguments]) of
				#cmdresult{gsMoveMade=false, canClaimDraw=CanClaimDraw} ->
					case core_gamestate:getState() of
						white_win ->
							xbi_controller:stdout("result 1-0 {White mates}");
						black_win ->
							xbi_controller:stdout("result 0-1 {Black mates}");
						draw ->
							case CanClaimDraw of
								true ->
									xbi_controller:stdout("result 1/1-1/2 {Draw by repetition}");
								false ->
									xbi_controller:stdout("result 1/1-1/2 {Stalemate}")
							end
					end;
				#cmdresult{gsMoveMade=true, canClaimDraw=CanClaimDraw} ->
					Current = core_gamestate:getCurrentNode(),
					Preceding = core_gamestate:getPrecedingNode(),
					
					timer:sleep(1800),    %% TODO, something better

					case CanClaimDraw of
						true ->
							% tested by an Xboard game
							xbi_controller:stdout("offer draw");
						false ->
							ok
					end,
					
					xbi_controller:stdout("move " ++ core_move:toAlgebraic(core_move:create(Preceding, Current))),
				
					xbi_controller:log("black advantage: ~w", [Current#node.blackAdvantage]),
					
					case core_gamestate:getState() of
						white_win ->
							xbi_controller:stdout("result 1-0 {White mates}");
						black_win ->
							xbi_controller:stdout("result 0-1 {Black mates}");
						draw ->
							case CanClaimDraw of
								true ->
									xbi_controller:stdout("result 1/1-1/2 {Draw by repetition}");
								false ->
									xbi_controller:stdout("result 1/1-1/2 {Stalemate}")
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

translate(Alg, #node{pieces=BoardMap, toMove=ToMove}) ->
	FromSquareName = string:substr(Alg, 1, 2),
	ToSquareName = string:substr(Alg, 3, 2),
	FromSquare = core_board:getSquare(FromSquareName),
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
	
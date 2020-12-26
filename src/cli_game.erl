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
%% Created: Sep 26, 2011
%% Description: TODO: Add description to game
%% TODO: Hard-coded constant "4" in play/1 and suggest/1.
%% TODO: Hard-coded constant 7 in display()

-module(cli_game).

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
%% Exported Functions
%%


-export([preMain/0]).

-export([main/1]).
-export([play/4]).

-export([setupBoard/0]).



%%
%% API Functions
%%



	


%% @doc Tentative, start a game with trace. The trace
%% is extensive ...

preMain() ->
	Pid = spawn_link(?MODULE, main, [["debug"]]),
	erlang:trace_pattern({'_','_','_'}, true, []),
	erlang:trace(Pid, true, [call]),
	Pid ! go,
	preMainLoop().
	
preMainLoop() ->
	receive
		{trace, P, Tag, Data1} ->
			io:fwrite("trace: ~w ~w ~w~n", [P, Tag, Data1]);
		{trace, P, Tag, Data1, Data2} ->
			io:fwrite("trace: ~w ~w ~w ~w~n", [P, Tag, Data1, Data2])
	end,
	preMainLoop().



%% @doc Play a game.

-spec main([string()]) -> #gameState{}.

main(["run", LibDir, Mode]) ->
	case core_util:initVm(LibDir, list_to_atom(Mode)) of
		{error, X} ->
			io:format("warning: ~p, expect reduced performance~n", [X]);
		{ok} ->
			io:format("successfully loaded native functions~n", [])
	end,
	play(false, false, standard_io, user);


%% @doc Untested and unsupported.

main(["debug"]) ->
	receive
		go ->
			ok
	end,
	play(false, false, standard_io, user).

	
setupBoard() ->
	put(board, core_board:instance()).


	




%% @doc Initialize, play a game, clean up.
%%
%% TODO, what are the different entry sequences?


-spec play(boolean(), boolean(), xdevice(), o_device()) -> #gameState{}.

play(Echo, BreakOnException, InputDevice, ResultDevice) ->
	core_state:clear(),
	param_parameter:setNumberOfThreads(?MAX_THREADS_DEFAULT),
	core_state:sput(mode, console),
	core_gamestate:create(),
	setupBoard(),
	
	core_state:reset(loopCount),
	cmd_play:setSuggestion(?SMALLINT_MIN_VALUE, null),
	
	cmd_dict:setupCommands(),
	
	try playLoop(Echo, InputDevice, BreakOnException, ResultDevice) of
		ok ->
			{ok, GameState} = core_state:sget(gameState),
			GameState
	catch
		throw:(#exception{type=userException}) ->
			throw({throw, core_gamestate:getCurrentNode(), null});
		throw:X:Stack ->
			erlang:display(Stack),
			{ok, GameState} = core_state:sget(gameState),
			throw({throw, GameState, X});
		error:X:Stack ->
			erlang:display(Stack),
			throw({error, X});
		exit:X:Stack ->
			erlang:display(Stack),
			throw({exit, X})
	end.




%% @doc The read-execute loop of an ongoing game.

-spec playLoop(boolean(), xdevice(), boolean(), o_device()) -> ok.

playLoop(Echo, InputDevice, BreakOnException, ResultDevice) ->
	
	core_state:incrementAndGet(loopCount, 1),
	
	case io:get_line(InputDevice, ?CLI_PROMPT) of
		eof ->
			core_util:inconsistencyException("unexpected end-of-file on input");
		{error, _} ->
			core_util:inconsistencyException("unexpected error on input");
		Line ->
			if
				Echo ->
					io:fwrite(ResultDevice, "~s~s", [?CLI_PROMPT, Line]);
				true ->
					ok
			end,
			case string:tokens(Line, " \t\n") of
				[] ->
					playLoop(Echo, InputDevice, BreakOnException, ResultDevice);
				[CommandName | Arguments] ->
					try apply(cmd_dict:getFunction(CommandName, Arguments), [Arguments]) of
						#cmdresult{proceed=false} ->
							ok;
						
						#cmdresult{proceed=true}=CR ->

							display(ResultDevice, CR),
							playLoop(Echo, InputDevice, BreakOnException, ResultDevice);
						
						Other-> % TODO, eliminate after static analysis
							core_util:inconsistencyException("ill-formed function result: ~w~n", [Other])
					catch
						throw: E ->
							case E of
								#exception{type=userException} ->
									
									cli_frame:display(ResultDevice, cli_frame:wrapException(E)),
									case BreakOnException of
										true ->
											throw(E);
										_ ->
											playLoop(Echo, InputDevice, BreakOnException, ResultDevice)
									end;
								#exception{type=inconsistencyException} ->
									cli_frame:display(ResultDevice, cli_frame:wrapException(E)),
									throw(E);
								_ ->
									io:fwrite(standard_error, "caught: ~p~n", [E]),
									throw(E)
							end;
						exit:Y:Stack ->
							io:fwrite(standard_error, "caught exit: ~w~n", [Y]),
							io:fwrite(standard_error, "stack trace: ~p~n", [Stack]),
							exit(Y);
						error:Z:Stack ->
							io:fwrite(standard_error, "caught error: ~w~n", [Z]),
							io:fwrite(standard_error, "stack trace: ~p~n", [Stack]),
							error(Z)
					end
			
			end
	end.




%% -spec describeMoveTo(#node{}, #node{}, boolean()) -> string().
%% 
%% describeMoveTo(#node{toMove=ToMove}=N1, N2, Future) ->
%% 	Move = core_move:create(N1, N2),
%% 	Player = atom_to_list(ToMove),
%% 	S1 = Player ++ (if Future -> " may move: "; true -> " moved: " end) ++ core_move:toString(Move),
%% 	OtherPlayer = core_colour:otherColour(ToMove),
%% 	Loss = loss(N1, OtherPlayer, N2),
%% 	case Loss of
%% 		null ->
%% 			S2 = S1;
%% 		#piece{type=Type, square=#square{name=SquareName}} ->
%% 			#pieceType{rname=RName} = core_piecetype:pieceType(Type),
%% 			S2 = S1++", "++atom_to_list(OtherPlayer)++
%% 					 (if Future ->
%% 							 " will lose " ++ RName ++ " at " ++ SquareName;
%% 						 true ->
%% 							 " lost " ++ RName ++ " at " ++ SquareName
%% 					  end)
%% 	end,
%% 	
%% 	case core_node:isChecked(N2) of
%% 		true ->
%% 			S2 ++ " - CHECK!";
%% 		_ ->
%% 			S2
%% 	end.



%% @doc Displays the state of the game.
%%
%% TODO, hardcoded values 0 (several places) and 4
%% TODO, sharpen the type of 1st argument

-spec display(term(), #cmdresult{}) -> ok.

display(Out, #cmdresult{message=Message, text=Text}) ->
	Frame = cli_frame:create(?CLI_INDENT, ?CLI_TOP, ?CLI_BOTTOM),

	Frame2 =
		case Text of
			[] ->
				Frame;
			_ ->
				%% block of text
				cli_frame:addRectangle(cli_rectangle:create(?CLI_RIGHT_COL,  ?CLI_RIGHT_FLOOR1, Text), Frame)
		end,

	Summary2 =
		case Message of
			null ->
				[];
			_ ->
				[Message]
		end,
	
	case core_gamestate:isSetup() of
		false ->
			Summary5 = lists:append(Summary2, [core_gamestate:summary()]),
			
			%% summary
			Frame3 = cli_frame:addRectangle(cli_rectangle:create(?CLI_RIGHT_COL, 0, Summary5), Frame2),
			cli_frame:display(Out, Frame3);
		
		_ ->
			CurrentNode = core_gamestate:getCurrentNode(),
			
			%% chessboard display
			Frame3 = cli_frame:addRectangle(cli_rectangle:create(0, 0, core_node:toStringList(CurrentNode)), Frame2),
			PrecedingNode = core_gamestate:getPrecedingNode(),
			
			Summary3 = 
			if
				PrecedingNode =/= null ->
					Move = core_move:create(PrecedingNode, CurrentNode),
					MoveDescription = core_move:describe(Move, false),
					lists:append(Summary2, [MoveDescription]);
				true ->
					Summary2
			end,
			
			Summary4 = lists:append(Summary3, [core_gamestate:summary()]),
			MoveNumber = (core_gamestate:getSize()+1) div 2,
			#node{toMove=ToMove} = CurrentNode,
			Summary5 = lists:append(Summary4, ["Move: " ++ integer_to_list(MoveNumber) ++ ", " ++
						   atom_to_list(ToMove)++" to move"]),
			
			%% summary
			Frame4 = cli_frame:addRectangle(cli_rectangle:create(?CLI_RIGHT_COL, 0, Summary5), Frame3),
			cli_frame:display(Out, Frame4)
	end.
	
	
	
	
	

%% 
%% 
%% 
%% 
%% 
%% 		
%% 
%% -spec loss(#node{}, colour(), #node{}) -> ppiece().
%%         
%% loss(#node{white=W, black=B}, Player, NewNode) -> 
%% 	M = (if Player =:= white -> W; true -> B end),
%% 	lossHelper(M, Player, NewNode).
%% 	
%% 
%% 
%% -spec lossHelper([#piece{}], colour(), #node{}) -> ppiece().
%% 
%% lossHelper([], _Player, _NewNode) ->
%% 	null;
%% 
%% lossHelper([P|Tail], Player, NewNode) ->
%% 	case stillAround(Player, P, NewNode) of
%% 		true ->
%% 			lossHelper(Tail, Player, NewNode);
%% 		_ ->
%% 			P
%% 	end.

%% 
%% -spec stillAround(colour(), #piece{}, #node{}) -> boolean().
%% 
%% stillAround(Colour, #piece{square=S}, #node{pieces=BoardMap}) ->
%% 	M = core_boardmap:get(S, BoardMap),
%% 	case M of
%% 		null ->
%% 			false;
%% 		#piece{colour=Colour} ->
%% 			true;
%% 		_ ->
%% 			false
%% 	end.





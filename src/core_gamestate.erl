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
%% Created: Jan 13, 2012
%% Description: Encapsulation of GameState.
-module(core_gamestate).
	
%%
%% Include files
%%

-include("antonius.hrl").
-include("piecetype.hrl").
-include("board.hrl").
-include("boardmap.hrl").
-include("node.hrl").
-include("gameState.hrl").


-include_lib("eunit/include/eunit.hrl").





%%
%% Exported Functions
%%
-export([create/0]).
-export([destroy/0]).
-export([hasNodes/0]).
-export([getCurrentNode/0]).
-export([getCurrentNode/1]).
-export([getPrecedingNode/0]).
-export([addNode/1]).
-export([addNode/2]).
-export([removeCurrentNode/0]).

-export([putAbResult/3]).
-export([getAbResult/2]).
-export([getAbResult/1]).

-export([clear/0]).
-export([isSetup/0]).
-export([getSize/0]).
-export([toString/1]).

-export([getState/0]).


-export([setCurrentState/1]).

-export([checkGameOpen/0]).

-export([getRepetitions/0]).
-export([getRepeatCount/1]).

-export([summary/0]).

-export([getHistory/0]).

%%
%% API Functions
%%


-spec create() -> ok.

create() ->
	core_state:sput(gameState, #gameState{nodes=[], abResults=[], repeatCount=dict:new()}).


-spec destroy() -> ok.

destroy() ->
	core_state:remove(gameState).


-spec getCurrentNode() -> #node{}.

getCurrentNode() ->
	case getGameState() of
		#gameState{nodes=[]} ->
			throw("this game has no nodes");
		#gameState{nodes=[#statenode{node=Node}|_]} ->
			Node
	end.


%% @doc No callers yet.

-spec hasNodes() -> boolean().

hasNodes() ->
	case getGameState() of
		#gameState{nodes=[]} ->
			false;
		_ ->
			true
	end.


%% @doc  TODO, is this really needed? Perhaps we kill the state dictionary too soon.
%% TODO, join with method above?

-spec getCurrentNode(#gameState{}) -> #node{}.

getCurrentNode(#gameState{nodes=[]}) ->
	core_util:inconsistencyException("cannot get node from empty gamestate", []);

getCurrentNode(#gameState{nodes=[#statenode{node=Node}|_]}) ->
	Node.


-spec getPrecedingNode() -> #node{} | null.

getPrecedingNode() ->
	getPrecedingNode(getGameState()).



-spec getPrecedingNode(#gameState{}) -> #node{} | null.

getPrecedingNode(#gameState{nodes=[]}) ->
	throw("this game has no nodes");

getPrecedingNode(#gameState{nodes=[_]}) ->
	null;

getPrecedingNode(#gameState{nodes=[_, #statenode{node=Node}|_]}) ->
	Node.


-spec addNode(#node{}) -> ok.

addNode(Node) ->
	addNode(Node, undefined).


%% @doc Add a node when state is known.

-spec addNode(#node{}, state()) -> ok.

addNode(Node, State) ->
	case core_state:sget(gameState) of
		null ->
			create(),
			addNode(Node, State);
		{ok, #gameState{nodes=Nodes, abResults=KeyList, repeatCount=R}} ->
			Key = core_node:key(Node),
			case dict:find(Key, R) of
				error ->
					core_state:sput(gameState, #gameState{nodes=[core_statenode:create(Node, State)|Nodes],
													      abResults=KeyList,
													      repeatCount=dict:store(Key, 1, R)});
				{ok, Count} ->
					core_state:sput(gameState, #gameState{nodes=[core_statenode:create(Node, State)|Nodes],
													      abResults=KeyList,
													      repeatCount=dict:store(Key, Count + 1, R)})
			end
	end.






-spec removeCurrentNode() -> ok.

removeCurrentNode() ->
	case getGameState() of
		#gameState{nodes=[]} ->
			core_util:inconsistencyException("no nodes to remove", []);
		#gameState{nodes=[#statenode{node=Node}|Tail], abResults=KeyList, repeatCount=R} ->
			Key = core_node:key(Node),
			case dict:find(Key, R) of
				error ->
					core_util:inconsistencyException("removeCurrentNode: no repeat count for key: ~s", [Key]);
				{ok, 1} ->
					core_state:sput(gameState, #gameState{nodes=Tail, abResults=KeyList, repeatCount=dict:erase(Key, R)});
				{ok, Count} ->
					core_state:sput(gameState, #gameState{nodes=Tail, abResults=KeyList, repeatCount=dict:store(Key, Count - 1, R)})
			end
	end.



%% @doc Store an alpha-beta result in the game state.

-spec putAbResult(string(), smallint(), #abResult{}) -> ok.

putAbResult(Key, Depth, ABResult) ->
	#gameState{nodes=Nodes, abResults=KeyList, repeatCount=Dict} = getGameState(),
	case lists:keyfind(Key, 1, KeyList) of
		false ->
			core_state:sput(gameState, #gameState{nodes=Nodes, abResults=[{Key, [{Depth, ABResult}]}|KeyList], repeatCount=Dict});
		{_, Map} ->
			core_state:sput(gameState, #gameState{nodes=Nodes, abResults=lists:keystore(Depth, 1, Map, {Depth, ABResult}), repeatCount=Dict})
	end.


-spec getAbResult(string(), smallint()) -> #abResult{} | null. 

getAbResult(Key, Depth) ->
	#gameState{abResults=KeyList} = getGameState(),
	case lists:keyfind(Key, 1, KeyList) of
		false ->
			null;
		{_, Map} ->
			case lists:keyfind(Depth, 1, Map) of
				false ->
					null;
				{_, ABResult} ->
					ABResult
			end
	end.


-spec getAbResult(string()) -> #abResult{} | null.

getAbResult(Key) ->
	#gameState{abResults=KeyList} = getGameState(),
	case lists:keyfind(Key, 1, KeyList) of
		false ->
			null;
		{_, Map} ->
			{_, ABResult} = lists:last(lists:keysort(1, Map)),
			ABResult
	end.
				
	



-spec clear() -> ok.

clear() ->
	create().


-spec isSetup() -> boolean().

isSetup() ->
	getSize() > 0.

-spec getSize() -> smallint().

getSize() ->
	#gameState{nodes=Nodes} = getGameState(),
	length(Nodes).



summary() ->
	"game state: "++
		atom_to_list(getState())++
		", depth: "++
		integer_to_list(param_parameter:getRecursionDepth())++
		", threads: "++
		integer_to_list(param_parameter:getNumberOfThreads()).


	

-spec toString(#gameState{}) -> string().

toString(_) ->
	"TBD: a GameState string".



%% @doc Returns one of init, open, draw, white_win, black_win, undefined

-spec getState() -> state().

getState() ->
	case getGameState() of
		#gameState{nodes=[]} ->
			init;
		#gameState{nodes=[#statenode{state=State}|_]} ->
			State
	end.



%% @doc It is trusted that there is a current node.

-spec setCurrentState(state()) -> ok.

setCurrentState(State) ->
	#gameState{nodes=[#statenode{node=Node}|Tail],
			    abResults=AbResults,
			   repeatCount=Dict} = getGameState(),
	core_state:sput(gameState, #gameState{nodes=[core_statenode:create(Node, State)|Tail],
									       abResults=AbResults,
									      repeatCount=Dict}).



%% 	protected void checkGameOpen(GameState gs) {
%% 		if (gs.getState() != GameState.State.OPEN) {
%% 			throw new UserException("game is not open");
%% 		}
%% 	}


-spec checkGameOpen() -> ok.

checkGameOpen() ->
	case getState() of
		open ->
			ok;
		Other ->
			core_util:inconsistencyException("expected game state: open, actual: ~w", [Other])
	end.



%% 
%% 	public List<String> getRepetitions() {
%% 		final List<String> result = new ArrayList<String>();
%% 		for (Map.Entry<String, Integer> e : repeatCount.entrySet()) {
%% 			if (e.getValue().intValue() >= 3) {
%% 				result.add(e.getKey());
%% 			}
%% 		}
%% 		return result;
%% 	}



-spec getRepetitions() -> [string()].
		  
getRepetitions() ->
	#gameState{repeatCount=Dict} = getGameState(),
	[Key || {Key, Count} <- dict:to_list(Dict), Count >= 3].



-spec getRepeatCount(string()) -> smallint().
	
getRepeatCount(Key) ->
	#gameState{repeatCount=Dict} = getGameState(),
	case dict:find(Key, Dict) of
		error ->
			0;
		{ok, Count} ->
			Count
	end.



-spec getHistory() ->  [#statenode{}].

getHistory() -> 
	#gameState{nodes=StateNodes} = getGameState(),
	lists:reverse(StateNodes).



%%
%% Local Functions
%%

-spec getGameState() -> #gameState{}.

getGameState() ->
	case core_state:sget(gameState) of
		null ->
			throw("gameState does not exist");
		{ok, GameState} ->
			GameState
	end.


% --------
game_state_test() ->
	
	NodeA = core_node:create("*Ke1", "*Ke8", white),
	NodeB = core_node:create("Ke2", "*Ke8", black),
	
	core_state:start(), core_state:init([]),
	
	create(),
	?assertEqual(0, getSize()),
	?assertEqual(false, isSetup()),
	addNode(NodeA),
	addNode(NodeB),
	?assertEqual(2, getSize()),
	?assertEqual(true, isSetup()),
	?assertEqual(NodeB, getCurrentNode()),
	?assertEqual(NodeA, getPrecedingNode()),
	removeCurrentNode(),
	?assertEqual(1, getSize()),
	?assertEqual(NodeA, getCurrentNode()),
	
	try getPrecedingNode() of
		_ ->
			shouldNotHappen
	catch
		throw:X ->
			?assertEqual(true, is_list(X)),
			?assertEqual(X, "this game has no preceding node")
	end,

	removeCurrentNode(),
	try getCurrentNode() of
		_ ->
			shouldNotHappen
	catch
		throw:Y ->
			?assertEqual(true, is_list(Y)),
			?assertEqual(Y, "this game has no nodes")
	end,
	
	try removeCurrentNode() of
		_ ->
			shouldNotHappen
	catch
		throw:Z ->
			?assertEqual(true, is_list(Z)),
			?assertEqual(Z, "no nodes to remove")
	end,
		
	% Add a couple of nodes, then destroy the GameState,
	% verify that "current node" cannot be accessed.
	
	NodeX = core_node:create("Ke2", "Ke7", white),
	NodeY = core_node:create("Ke3", "Ke7", black),
	NodeZ = core_node:create("Ke3", "Ke6", white),
	
	addNode(NodeX),
	addNode(NodeY),
	addNode(NodeZ),
	
	destroy(),
	
	try getCurrentNode() of
		_ ->
			shouldNotHappen
	catch
		throw:W ->
			?assertEqual(is_list(W), true),
			?assertEqual("gameState does not exist", W)
	end,
	
	core_state:stop().


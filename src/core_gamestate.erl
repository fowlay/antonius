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
-export([create/1]).
-export([destroy/1]).
-export([getCurrentNode/1]).
-export([hasNodes/1]).
-export([getCurrentNodeFromGameState/1]).
-export([getPrecedingNode/1]).
-export([getPrecedingNodeFromGameState/1]).
-export([addNode/2]).
-export([addNode/3]).
-export([removeCurrentNode/1]).

-export([putAbResult/4]).
-export([getAbResult/3]).
-export([getAbResult/2]).

-export([clear/1]).
-export([isSetup/1]).
-export([getSize/1]).
-export([toString/1]).

-export([getState/1]).


-export([setCurrentState/2]).

-export([checkGameOpen/1]).

-export([getRepetitions/1]).
-export([getRepeatCount/2]).

-export([summary/1]).

-export([getHistory/1]).

%%
%% API Functions
%%


-spec create(sid()) -> ok.

create(Sid) ->
	core_state:sput({Sid, gameState}, #gameState{nodes=[], abResults=[], repeatCount=dict:new()}).


-spec destroy(sid()) -> ok.

destroy(Sid) ->
	core_state:remove({Sid, gameState}).


-spec getCurrentNode(sid()) -> #node{}.

getCurrentNode(Sid) ->
	case getGameState(Sid) of
		#gameState{nodes=[]} ->
			throw("this game has no nodes");
		#gameState{nodes=[#statenode{node=Node}|_]} ->
			Node
	end.


%% @doc No callers yet.

-spec hasNodes(sid()) -> boolean().

hasNodes(Sid) ->
	case getGameState(Sid) of
		#gameState{nodes=[]} ->
			false;
		_ ->
			true
	end.


%% @doc  TODO, is this really needed? Perhaps we kill the state dictionary too soon.
%% TODO, join with method above?

-spec getCurrentNodeFromGameState(#gameState{}) -> #node{}.

getCurrentNodeFromGameState(#gameState{nodes=[]}) ->
	core_util:inconsistencyException("cannot get node from empty gamestate", []);

getCurrentNodeFromGameState(#gameState{nodes=[#statenode{node=Node}|_]}) ->
	Node.


-spec getPrecedingNode(sid()) -> #node{} | null.

getPrecedingNode(Sid) ->
	getPrecedingNodeFromGameState(getGameState(Sid)).



-spec getPrecedingNodeFromGameState(#gameState{}) -> #node{} | null.

getPrecedingNodeFromGameState(#gameState{nodes=[]}) ->
	throw("this game has no nodes");

getPrecedingNodeFromGameState(#gameState{nodes=[_]}) ->
	null;

getPrecedingNodeFromGameState(#gameState{nodes=[_, #statenode{node=Node}|_]}) ->
	Node.


-spec addNode(sid(), #node{}) -> ok.

addNode(Sid, Node) ->
	addNode(Sid, Node, undefined).


%% @doc Add a node when state is known.

-spec addNode(sid(), #node{}, state()) -> ok.

addNode(Sid, Node, State) ->
	case core_state:sget({Sid, gameState}) of
		null ->
			create(Sid),
			addNode(Node, State);
		{ok, #gameState{nodes=Nodes, abResults=KeyList, repeatCount=R}} ->
			Key = core_node:key(Node),
			case dict:find(Key, R) of
				error ->
					core_state:sput({Sid, gameState}, #gameState{nodes=[core_statenode:create(Node, State)|Nodes],
													      abResults=KeyList,
													      repeatCount=dict:store(Key, 1, R)});
				{ok, Count} ->
					core_state:sput({Sid, gameState}, #gameState{nodes=[core_statenode:create(Node, State)|Nodes],
													      abResults=KeyList,
													      repeatCount=dict:store(Key, Count + 1, R)})
			end
	end.






-spec removeCurrentNode(sid()) -> ok.

removeCurrentNode(Sid) ->
	case getGameState(Sid) of
		#gameState{nodes=[]} ->
			core_util:inconsistencyException("no nodes to remove", []);
		#gameState{nodes=[#statenode{node=Node}|Tail], abResults=KeyList, repeatCount=R} ->
			Key = core_node:key(Node),
			case dict:find(Key, R) of
				error ->
					core_util:inconsistencyException("removeCurrentNode: no repeat count for key: ~s", [Key]);
				{ok, 1} ->
					core_state:sput({Sid, gameState}, #gameState{nodes=Tail, abResults=KeyList, repeatCount=dict:erase(Key, R)});
				{ok, Count} ->
					core_state:sput({Sid, gameState}, #gameState{nodes=Tail, abResults=KeyList, repeatCount=dict:store(Key, Count - 1, R)})
			end
	end.



%% @doc Store an alpha-beta result in the game state.

-spec putAbResult(sid(), string(), smallint(), #abResult{}) -> ok.

putAbResult(Sid, Key, Depth, ABResult) ->
	#gameState{nodes=Nodes, abResults=KeyList, repeatCount=Dict} = getGameState(Sid),
	case lists:keyfind(Key, 1, KeyList) of
		false ->
			core_state:sput({Sid, gameState}, #gameState{nodes=Nodes, abResults=[{Key, [{Depth, ABResult}]}|KeyList], repeatCount=Dict});
		{_, Map} ->
			core_state:sput({Sid, gameState}, #gameState{nodes=Nodes, abResults=lists:keystore(Depth, 1, Map, {Depth, ABResult}), repeatCount=Dict})
	end.


-spec getAbResult(sid(), string(), smallint()) -> #abResult{} | null. 

getAbResult(Sid, Key, Depth) ->
	#gameState{abResults=KeyList} = getGameState(Sid),
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


-spec getAbResult(sid(), string()) -> #abResult{} | null.

getAbResult(Sid, Key) ->
	#gameState{abResults=KeyList} = getGameState(Sid),
	case lists:keyfind(Key, 1, KeyList) of
		false ->
			null;
		{_, Map} ->
			{_, ABResult} = lists:last(lists:keysort(1, Map)),
			ABResult
	end.
				
	



-spec clear(sid()) -> ok.

clear(Sid) ->
	create(Sid).


-spec isSetup(sid()) -> boolean().

isSetup(Sid) ->
	getSize(Sid) > 0.

-spec getSize(sid()) -> smallint().

getSize(Sid) ->
	#gameState{nodes=Nodes} = getGameState(Sid),
	length(Nodes).



summary(Sid) ->
	"game state: "++
		atom_to_list(getState(Sid))++
		", depth: "++
		integer_to_list(param_parameter:getRecursionDepth(Sid))++
		", threads: "++
		integer_to_list(?MAX_THREADS_DEFAULT).


	

-spec toString(#gameState{}) -> string().

toString(_) ->
	"TBD: a GameState string".



%% @doc Returns one of init, open, draw, white_win, black_win, undefined

-spec getState(sid()) -> state().

getState(Sid) ->
	case getGameState(Sid) of
		#gameState{nodes=[]} ->
			init;
		#gameState{nodes=[#statenode{state=State}|_]} ->
			State
	end.



%% @doc It is trusted that there is a current node.

-spec setCurrentState(sid(), state()) -> ok.

setCurrentState(Sid, State) ->
	#gameState{nodes=[#statenode{node=Node}|Tail],
			    abResults=AbResults,
			   repeatCount=Dict} = getGameState(Sid),
	core_state:sput({Sid, gameState}, #gameState{nodes=[core_statenode:create(Node, State)|Tail],
									       abResults=AbResults,
									      repeatCount=Dict}).



-spec checkGameOpen(sid()) -> ok.

checkGameOpen(Sid) ->
	case getState(Sid) of
		open ->
			ok;
		Other ->
			core_util:inconsistencyException("expected game state: open, actual: ~w", [Other])
	end.






-spec getRepetitions(sid()) -> [string()].
		  
getRepetitions(Sid) ->
	#gameState{repeatCount=Dict} = getGameState(Sid),
	[Key || {Key, Count} <- dict:to_list(Dict), Count >= 3].



-spec getRepeatCount(sid(), string()) -> smallint().
	
getRepeatCount(Sid, Key) ->
	#gameState{repeatCount=Dict} = getGameState(Sid),
	case dict:find(Key, Dict) of
		error ->
			0;
		{ok, Count} ->
			Count
	end.



-spec getHistory(sid()) ->  [#statenode{}].

getHistory(Sid) -> 
	#gameState{nodes=StateNodes} = getGameState(Sid),
	lists:reverse(StateNodes).



%%
%% Local Functions
%%

-spec getGameState(sid()) -> #gameState{}.

getGameState(Sid) ->
	case core_state:sget({Sid, gameState}) of
		null ->
			throw("gameState does not exist");
		{ok, GameState} ->
			GameState
	end.


% --------
game_state_test() ->
	
	Sid = test,
	
	NodeA = core_node:create("*Ke1", "*Ke8", white),
	NodeB = core_node:create("Ke2", "*Ke8", black),
	
	core_state:start(),
	
	create(Sid),
	?assertEqual(0, getSize(Sid)),
	?assertEqual(false, isSetup(Sid)),
	addNode(Sid, NodeA),
	addNode(Sid, NodeB),
	?assertEqual(2, getSize(Sid)),
	?assertEqual(true, isSetup(Sid)),
	?assertEqual(NodeB, getCurrentNode(Sid)),
	?assertEqual(NodeA, getPrecedingNode(Sid)),
	removeCurrentNode(Sid),
	?assertEqual(1, getSize(Sid)),
	?assertEqual(NodeA, getCurrentNode(Sid)),
	
	try getPrecedingNode(Sid) of
		_ ->
			shouldNotHappen
	catch
		throw:X ->
			?assertEqual(true, is_list(X)),
			?assertEqual(X, "this game has no preceding node")
	end,

	removeCurrentNode(Sid),
	try getCurrentNode(Sid) of
		_ ->
			shouldNotHappen
	catch
		throw:Y ->
			?assertEqual(true, is_list(Y)),
			?assertEqual(Y, "this game has no nodes")
	end,
	
	try removeCurrentNode(Sid) of
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
	
	addNode(Sid, NodeX),
	addNode(Sid, NodeY),
	addNode(Sid, NodeZ),
	
	destroy(Sid),
	
	try getCurrentNode(Sid) of
		_ ->
			shouldNotHappen
	catch
		throw:W ->
			?assertEqual(is_list(W), true),
			?assertEqual("gameState does not exist", W)
	end,
	
	core_state:stop().


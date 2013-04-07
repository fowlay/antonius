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
%% Created: Jan 21, 2012
%% Description:
%% Encapsulates a collection of alpha-beta results for nodes
%% and a summary string: {summary(), results()}
%% A node has a computed alpha-beta value. The collection is
%% a list of tuples:
%%
%%  [
%%     {ABValue, [Node1, Node2, ...]},
%%     ...
%%  ]
%%
%% Ordering of lists and sublists is ascending, in the sense
%% defined by the compareTo/2 function on nodes, and natural
%% order of ABValues.
%%
%% This module is not resource critical.

-module(core_abresult).

%%
%% Include files
%%

-include("antonius.hrl").
-include("boardmap.hrl").
-include("node.hrl").
-include("abresult.hrl").

%%
%% Exported Functions
%%
-export([create/2]).
-export([createSingle/2]).
-export([bestMoves/2]).
-export([add/3]).
-export([isEmpty/1]).
-export([getSummary/1]).
-export([selectNode/3]).

-export([iteratorLoop/3]).

-export([iterator/2]).
-export([hasNext/1]).
-export([next/1]).
-export([done/1]).


%%
%% API Functions
%%


	
	


-spec create([{smallint(), [#node{}]}], xstring()) -> #abResult{}.

create(NodeSets, Summary) ->
	#abResult{nodeSets=NodeSets, summary=Summary}.


-spec createSingle(#node{}, string()) -> #abResult{}.

createSingle(#node{blackAdvantage=A}=N, Summary) ->
	create([{A, [N]}], Summary).


%% @doc Awkward code in the last clause. A helper function is needed! TODO
%% The logic can be obtained from the previous version.

-spec add(#node{}, smallint(), #abResult{}) -> #abResult{}.

add(Node, Value, #abResult{nodeSets=S, summary=Summary}) ->
	create(addHelper(Node, Value, S), Summary).

	

bestMoves(#abResult{summary=Summary, nodeSets=NodeSets}, #node{toMove=ToMove}=Node) ->
	Iterator = iterator(NodeSets, ToMove),
	R = lists:reverse(bestMovesHelper(1, Iterator, Node, [])),
	case Summary of
		null ->
			R;
		_ ->
			[Summary | R]
	end.


bestMovesHelper(N, Iterator, _, R) when N > ?DISPLAY_LIMIT ->
	case hasNext(Iterator) of
		true ->
			done(Iterator),
			["..."|R];
		_ ->
			done(Iterator),
			R
	end;
			

bestMovesHelper(N, Iterator, Node, R) ->
	case hasNext(Iterator) of
		true ->
			{Score, ToNode} = next(Iterator),
			Move = core_move:create(Node, ToNode),
			Descr = core_move:describe(Move, true),
			bestMovesHelper(N+1, Iterator, Node, [io_lib:format("~w (~w) ~s", [N, Score, Descr])|R]);
		_ ->
			done(Iterator),
			R
	end.



-spec isEmpty(#abResult{}) -> boolean().

isEmpty(#abResult{nodeSets=[]}) ->
	true;

isEmpty(_) ->
	false.


%% TODO ... nowhere used?

-spec getSummary(#abResult{}) -> string().

getSummary(#abResult{summary=Summary}) ->
	Summary.




%% @doc Returns an iterator over the given list of
%% {Score, NodeSet} tuples. Scores are in ascending
%% order. If the given colour is white the iteration
%% will be over ascending scores, else descending.

-spec iterator([tuple()], colour()) -> pid().

iterator(L, black) ->
	iterator(lists:reverse(L), white);

iterator([], white) ->
	spawn(?MODULE, iteratorLoop, [[], [], 0]);
	
iterator([{Score, NodeSet}|MoreTuples], white) ->
	spawn(?MODULE, iteratorLoop, [MoreTuples, NodeSet, Score]).


%% @doc Returns true if the given iterator can deliver
%% another value.

-spec hasNext(pid()) -> boolean().

hasNext(Pid) ->
	Pid ! {hasNext, self()},
	receive
		true ->
			true;
		false ->
			false
	end.


%% @doc Returns another {Score, Node} tuple.

-spec next(pid()) -> tuple() | error.

next(Pid) ->
	Pid ! {next, self()},
	receive
		{ok, Node, Score} ->
			{Score, Node};
		error ->
			error
	end.


%% @doc This function should be called on an iterator
%% that is no longer needed.

-spec done(pid()) -> ok.

done(Pid) ->
	Pid ! done,
	ok.



%% @doc Process loop that implements iteration over an alpa-beta result.
%% It is trusted that node sets are non-empty.
%%
%% The loop must be terminated explicitly via a 'done' request.

-spec iteratorLoop([tuple()], [#node{}], smallint()) -> ok | error.

iteratorLoop(TupleMap, Nodes, Score) ->
	receive
		done ->
			ok;
		{hasNext, Caller} ->
			case Nodes of
				[] ->
					case TupleMap of
						[] ->	
							Caller ! false,
							iteratorLoop([], [], 0);
						_ ->
							Caller ! true,
							iteratorLoop(TupleMap, Nodes, Score)
					end;
				_ ->
					Caller ! true,
					iteratorLoop(TupleMap, Nodes, Score)
			end;
		{next, Caller} ->
			case Nodes of
				[] ->
					case TupleMap of
						[] ->
							Caller ! error;
						[{NewScore, [Node|MoreNodes]}|MoreTuples] -> 
							Caller ! {ok, Node, NewScore},
							iteratorLoop(MoreTuples, MoreNodes, NewScore)
					end;
				[Node|MoreNodes] ->
					Caller ! {ok, Node, Score},
					iteratorLoop(TupleMap, MoreNodes, Score)
			end
	end.






%% @doc Select a node from the given alpha-beta result.
%% The given index refers to the presentation order,
%% starting from 1.

-spec selectNode(#abResult{}, smallint(), colour()) -> #node{} | null.

selectNode(#abResult{nodeSets=NodeSets}, Index, white) ->
	selectNode(NodeSets, Index);

selectNode(#abResult{nodeSets=NodeSets}, Index, black) ->
	selectNode(lists:reverse(NodeSets), Index).


%% @doc Select a node from the given keylist of node sets.

-spec selectNode([tuple()], smallint()) -> #node{} | null.

selectNode([], _) ->
	null;

selectNode([{_, Nodes}|Tail], Index) when Index > length(Nodes) ->
	selectNode(Tail, Index - length(Nodes));

selectNode([{_, Nodes}|_], Index) ->
	lists:nth(Index, Nodes).


%%
%% Local Functions
%%

%% @doc Add the given node with the given black-advantage to the
%% given variations map and return the resulting map.

-spec addHelper(#node{}, smallint(), [{smallint(), [#node{}]}]) -> [{smallint(), [#node{}]}].

addHelper(Node, V, []) ->
	[{V, [Node]}];

addHelper(Node, V, [{X, _}|_]=NS) when V < X ->
	[{V, [Node]}|NS];

addHelper(Node, V, [{X, L}|Tail]) when V =:= X ->
	[{X, core_node:insert(Node, L)}|Tail];

addHelper(Node, V, [{X, L}|Tail]) ->
	[{X, L}|addHelper(Node, V, Tail)].

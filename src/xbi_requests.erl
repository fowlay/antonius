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
%% Created: Aug 19, 2012
%% Description: Started by the controller, holds
%% requests issued by Xboard.

-module(xbi_requests).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/1]).

%%
%% API Functions
%%

start(Master) ->
	Master ! {ok, self()},
	
	spawn_link(xbi_listener, start, [self()]),
	
	receive
		{ok} ->
			ok
	end,
	
	loop([], false, null).

%%
%% Local Functions
%%

loop(Queue, DeferredSubtract, DeferredCaller) ->
	receive
		{add, Line} ->
			case DeferredSubtract of
				true ->
					DeferredCaller ! {line, Line},
					case Line of
						"quit" ->
							true;
						_ ->
							loop(Queue, false, null)
					end;
				false ->
					NewQueue = lists:append(Queue, [Line]),
					loop(NewQueue, false, null)
			end;
					
		
		{subtract, Caller} ->
			case Queue of
				[Head|Tail] ->
					Caller ! {line, Head},
					case Head of
						"quit" ->
							true;
						_ ->
							loop(Tail, false, null)
					end;
				[] ->
					loop([], true, Caller)
			end
	end.
			
			
			


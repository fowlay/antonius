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
%% Description: TODO: Add description to xboardlistener
-module(xbi_listener).

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
	Master ! {ok},
	
	[Eol] = io_lib:nl(),
	loop(Master, Eol).
	
	

%%
%% Local Functions
%%

loop(Master, Eol) ->
	TerminatedLine = io:get_line(""),
	Line = string:strip(TerminatedLine, right, Eol),
	Master ! {add, Line},
	case Line of
		"quit" ->
			ok;
		_ ->
			loop(Master, Eol)
	end.

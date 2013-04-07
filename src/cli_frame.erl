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
%% Created: Oct 29, 2012
%% Description: TODO: Add description to cli_frame
-module(cli_frame).

%%
%% Include files
%%

-include("frame.hrl").
-include("exception.hrl").

%%
%% Exported Functions
%%
-export([create/3]).
-export([addRectangle/2]).
-export([display/2]).
-export([wrapException/1]).

%%
%% API Functions
%%

create(Indentation, Top, Bottom) ->
	#frame{indentation=Indentation, top=Top, bottom=Bottom}.


-spec addRectangle(#rectangle{}, #frame{}) -> #frame{}.

addRectangle(R, #frame{bottom=B, top=T, indentation=I, rectangles=RR}) ->
	NewRR = insert(R, RR),
	#frame{indentation=I, top=T, bottom=B, rectangles=NewRR}.

display(Out, #frame{indentation=I, top=T, bottom=B, rectangles=RR}) ->
	repeatedNewlines(Out, T),
	YMax = maxUpperY(RR, -999),
	displayIter(Out, YMax, I, RR),
	repeatedNewlines(Out, B).
	
	

%% TODO, hard code in screen layout

-spec wrapException(#exception{}) -> #frame{}.

wrapException(#exception{reason=Reason, message=Message}) ->
	Frame=#frame{indentation=5, top=1, bottom=1},
	addRectangle(cli_rectangle:create(0, 0, [Reason++":", Message]), Frame).

%%
%% Local Functions
%%


-spec insert(#rectangle{}, [#rectangle{}]) -> [#rectangle{}]. 

insert(R, []) ->
	[R];

insert(R, [U|Z]) ->
	C = cli_rectangle:compareTo(R, U),
	if
		C < 0 ->
			[R, U|Z];
		true ->
			[U|insert(R, Z)]
	end.


maxUpperY([], R) ->
	R;

maxUpperY([#rectangle{upperY=Y1}|Z], R) when Y1 > R ->
	maxUpperY(Z, Y1);

maxUpperY([_|Z], R) ->
	maxUpperY(Z, R).


displayIter(_, -1, _, _) ->
	ok;

displayIter(Out, Y, I, RR) ->
	rectIter(Out, 0, Y, I, RR),
	io:fwrite(Out, "\n", []),
	displayIter(Out, Y-1, I, RR).


rectIter(_, _, _, _, []) ->
	ok;

rectIter(Out, XPrevious, Y, I, [#rectangle{x=X, y=Y0, upperY=Y1, text=Text}|Z]) when Y >= Y0, Y =< Y1 ->
	if
		I > 0 ->
			repeatedSpaces(Out, I);
		true ->
			ok
	end,
	repeatedSpaces(Out, X - XPrevious),
	Segment = lists:nth(Y1-Y+1,Text),
	io:fwrite(Out, "~s", [Segment]),
	rectIter(Out, X+length(Segment), Y, 0, Z);

rectIter(Out, XPrevious, Y, I, [_|Z]) ->
	rectIter(Out, XPrevious, Y, I, Z).

repeatedNewlines(_, 0) ->
	ok;

repeatedNewlines(Out, N) ->
	io:fwrite(Out, "\n", []),
	repeatedNewlines(Out, N-1).
	

repeatedSpaces(_, 0) ->
	ok;

repeatedSpaces(Out, N) ->
	io:fwrite(Out, " ", []),
	repeatedSpaces(Out, N-1).
	

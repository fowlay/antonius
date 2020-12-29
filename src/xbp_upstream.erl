%% @author erarafo
%% @doc @todo Add description to xbp_upstream.


-module(xbp_upstream).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).


start(Master, Socket) ->
	case gen_server:start({local, ?MODULE}, ?MODULE, [Master, Socket], []) of
		{ok, Pid} ->
			xbp_downstream:say("upstream started: ~p", [Pid]),
			Pid
	end.




%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
		{master,
		 socket,
		 quit = false,
		 count = 0}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Master, Socket]) ->
    {ok, #state{master = Master, socket = Socket}, 0}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================

handle_info(timeout, #state{socket = Socket, quit = Quit, count = Count} = State) ->
	case io:get_line(standard_io, "") of
		eof when Quit ->
			%% TODO ..... unclear exactly how to sequence this
			gen_tcp:close(Socket),
			xbp_downstream:say("expected: eof from xboard", []),
			{stop, eof_from_xboard, State};
		eof ->
			gen_tcp:close(Socket),
			%% TODO, inform downstream too?
			xbp_downstream:say("unexpected: eof from xboard", []),
			{stop, eof_from_xboard, State};
		{error, Reason} ->
			gen_tcp:close(Socket),
			%% TODO, inform downstream too?
			xbp_downstream:say("error when reading from xboard, reason: ~p", [Reason]),
			{stop, {error, Reason}, State};
		DataRaw ->
			Command = string:trim(DataRaw),
			if Count < 2 -> timer:sleep(1000); true -> ok end,  %% TODO, ugly workaround;
            %% should be solved on server side
			case gen_tcp:send(Socket, Command ++ "\n") of
				{error, Reason} ->
					xbp_downstream:say("error when sending to server, reason: ~p", [Reason]),
					gen_tcp:close(Socket),
					%% TODO, inform downstream too?
					{stop, {error, Reason}, State};
				ok ->
					xbp_downstream:say("sent to server: ~p", [Command]),
					if
						Command =:= "quit" ->
							%% eof on input is expected next
							{noreply, State#state{quit = true, count = Count+1}, 0};
						true ->
							{noreply, State#state{count = Count+1}, 0}
					end
			end
	end;

handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================



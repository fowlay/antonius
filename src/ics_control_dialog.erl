%% @author erarafo
%% @doc Started by: ics_control
%% Server side of a telnet dialog



-module(ics_control_dialog).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2]).

start(Master, Socket) ->
	gen_server:start(?MODULE, [Master, Socket], []).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
		{master :: pid(),
		 socket :: inet:socket(),
		 databuffer :: list(),
		 commands = [] :: [string()],
		 shutdown = false :: boolean(),
		 close_at_once = false :: boolean()
		 }).

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
init([Master, Socket, CloseAtOnce]) ->
	inet:setopts(Socket, [{active, true}]),
    {ok, #state{master = Master, socket = Socket, close_at_once = CloseAtOnce}, 0}.


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

handle_info({tcp_closed, _Port}, State) ->
	%% telnet client closed session
	{stop, close, State};

handle_info({tcp, _Port, Data}, #state{databuffer = DataBuffer, commands = Commands} = State) when is_list(Data) ->
	%% incoming data; buffer it and time out immediately
	NewDataBuffer = DataBuffer ++ Data,
	{NewNewDataBuffer, NewCommands} = ics_lib:add_to_buffer(NewDataBuffer, Commands),
	{noreply, State#state{commands = NewCommands, databuffer = NewNewDataBuffer}, 0};

handle_info(timeout, #state{close_at_once = true} = State) ->
	%% can only occur if set by init
	{stop, close_at_once, State};

handle_info(timeout, #state{commands = []} = State) ->
	%% no commands yet
	{noreply, State};

handle_info(timeout, #state{commands = [First|More]} = State) ->
	NewState = execute(First, State),
	if
		NewState#state.shutdown ->
			{stop, shutdown, NewState};
		true ->
			{noreply, NewState#state{commands = More}, 0}
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


terminate(close_at_once, #state{master = Master, socket = Socket}) ->
	gen_tcp:send(Socket, "another session ongoing\r\n"),
	gen_tcp:close(Socket),
	gen_server:cast(Master, {close, self()}),
	ok;


terminate(close, #state{master = Master, socket = Socket}) ->
	gen_server:cast(Master, {close, self()}),
	gen_tcp:close(Socket),
	ok;

terminate(shutdown, #state{master = Master, socket = Socket}) ->
	gen_server:cast(Master, shutdown),
	gen_tcp:close(Socket),
    ok;


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

execute("shutdown", #state{socket = Socket} = State) ->
	gen_tcp:send(Socket, "ok\r\n"),
	gen_tcp:close(Socket),
	State#state{shutdown = true};

execute(_Unknown, #state{socket = Socket} = State) ->
	gen_tcp:send(Socket, "unknown command\r\n"),
	State.


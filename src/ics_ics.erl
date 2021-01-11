%% @author erarafo
%% @doc @todo Add description to ics_ics.


-module(ics_ics).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/1]).

start(ListenIcs) ->
	gen_server:start({local, ?MODULE}, ?MODULE, [ListenIcs], []).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
		{socket,
		 sessions = ordsets:new()}).

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
init([ListenIcs]) ->
    {ok, #state{socket = ListenIcs}, 0}.


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

handle_cast(stop, State) ->
	ordsets:fold(
	  fun(Pid, _Acc) ->
			  Pid ! stop       %% TODO, adaptation to xbi_controller
	  end,
	  ignore,
	  State#state.sessions),
	{stop, normal, State};

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
handle_info(timeout, State) ->
	case gen_tcp:accept(State#state.socket, 1000) of
		{error, timeout} ->
			%% time out periodically to handle control messages
			%% core_logger:logLine("accept timeout", []),
			{noreply, State, 0};
		{error, closed} ->
			core_logger:logLine("in module: ~p, gen_tcp:accept/2 returned {error, closed}", [?MODULE]),
			{noreply, State, 0};
		{ok, Socket} ->
			core_logger:logLine("connection accepted: ~p", [Socket]),
			ok = inet:setopts(Socket, [{active, false}]),
			
			Session = spawn(xbi_controller, start,
							[["/home/erarafo/git/antonius/lib", "ics", Socket, self()]]),
			wait_for_started(),
			ok = gen_tcp:controlling_process(Socket, Session),
			
			core_logger:logLine("connection passed to xbi_controller:start/1", []),
			NewSessions = ordsets:add_element(Session, State#state.sessions),
			{noreply, State#state{sessions = NewSessions}, 0}
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

wait_for_started() ->
	receive
		started ->
			ok;
		_ ->
			timer:sleep(100),   %% TODO, is this required at all??
			wait_for_started()
	end.


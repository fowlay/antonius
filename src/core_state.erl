%% @author erarafo
%% @doc @todo Add description to core_state.


-module(core_state).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0]).
-export([stop/0]).
-export([sget/1]).
-export([sput/2]).
-export([remove/1]).
-export([reset/1]).
-export([incrementAndGet/2]).
-export([dump/0]).
-export([clear/1]).

start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	true.

stop() ->
	gen_server:cast(?MODULE, {stop}).

sget(Key) ->
	gen_server:call(?MODULE, {sget, Key}).

sput(Key, Value) ->
	gen_server:call(?MODULE, {sput, Key, Value}).

remove(Key) ->
	gen_server:call(?MODULE, {remove, Key}).

reset(Counter) ->
	sput(Counter, 0).

incrementAndGet(Counter, Increment) ->
	gen_server:call(?MODULE, {incrementAndGet, Counter, Increment}).

dump() ->
	gen_server:call(?MODULE, {dump}).

clear(Sid) ->
	gen_server:call(?MODULE, {clear, Sid}).




%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, {dict}).

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
init([]) ->
    {ok, #state{dict = dict:new()}}.


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

handle_call({sget, Key}, _From, #state{dict=Dict} = State) ->
	Result =
		case dict:find(Key, Dict) of
			{ok, Value} ->
				{ok, Value};
			error ->
				null
		end,
    {reply, Result, State};

handle_call({sput, Key, Value}, _From, #state{dict=Dict} = State) ->
	NewDict = dict:store(Key, Value, Dict),
	{reply, ok, State#state{dict=NewDict}};

handle_call({remove, Key}, _From, #state{dict=Dict} = State) ->
	NewDict = dict:erase(Key, Dict),
	{reply, ok, State#state{dict=NewDict}};

handle_call({incrementAndGet, Counter, Increment}, _From, #state{dict=Dict} = State) ->
	case dict:find(Counter, Dict) of
		{ok, Value} ->
			NewValue = Value + Increment,
			NewDict = dict:store(Counter, NewValue, Dict),
			{reply, {ok, NewValue}, State#state{dict=NewDict}};
		error ->
			{reply, null, State}
	end;

handle_call({dump}, _From, State) ->
	{reply, State, State};

handle_call({clear, Sid}, _From, #state{dict=Dict} = State) ->
	NewDict =
		dict:fold(
		  fun({X, _Y}, _Value, Acc) when X =:= Sid ->
				  Acc;
			 (Key, Value, Acc) ->
				  dict:store(Key, Value, Acc)
		  end,
		  dict:new(),
		  Dict),
	
	{reply, ok, State#state{dict = NewDict}};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.


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



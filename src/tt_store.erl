-module(tt_store).
-author("Adam Szlachta").

-behaviour(gen_server).

-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([start_link/0, add/4, find_by_time/2, find_by_name/1, list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, talks).

-record(talk,  {title :: string(), start_time :: calendar:datetime(), end_time :: calendar:datetime(), location :: string()}).
-record(state, {table :: ets:tab()}).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self()], []).

-spec add(Title :: string, StartTime :: calendar:datetime(), EndTime :: calendar:datetime(), Where :: string) -> ok.
add(Title, StartTime, EndTime, Where) when StartTime =< EndTime ->
    gen_server:cast(?SERVER, {add, Title, StartTime, EndTime, Where}),
    ok;
add(_Title, _StartTime, _EndTime, _Where) ->
    error.

-spec find_by_time(StartTime :: calendar:datetime(), EndTime :: calendar:datetime()) -> [#talk{}].
find_by_time(StartTime, EndTime) ->
    gen_server:call(?SERVER, {find_by_time, StartTime, EndTime}).

-spec list() -> [#talk{}].
list() ->
    gen_server:call(?SERVER, list).

-spec find_by_name(Name :: string) -> [#talk{}].
find_by_name(Name) ->
    gen_server:call(?SERVER, {find_by_name, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, State :: #state{}}.
init([ParentPid]) ->
    process_flag(trap_exit, true),
    maybe_ets(?TABLE, ParentPid),
    {ok, #state{table = ?TABLE}}.

-spec handle_call(list
    | {find_by_time, StartTime :: calendar:datetime(), EndTime :: calendar:datetime()}
    | {find_by_name, Name :: string},
    From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, [#talk{}], NewState :: #state{}}.
handle_call(list, _From, State) ->
    Records = get_records(State#state.table),
    {reply, Records, State};
handle_call({find_by_time, StartTime, EndTime}, _From, State) ->
    Records = find_records_by_time(State#state.table, {StartTime, EndTime}),
    {reply, Records, State};
handle_call({find_by_name, Name}, _From, State) ->
    Records = find_records_by_name(State#state.table, Name),
    {reply, Records, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

-spec handle_cast({add, Title :: string, StartTime :: calendar:datetime(), EndTime :: calendar:datetime(), Where :: string},
    State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_cast({add, Title, StartTime, EndTime, Where}, State) ->
    insert_data(State#state.table, {Title, StartTime, EndTime, Where}),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info({'EXIT', _, {time_keeper_cancelled | time_keeper_finished, reference()}}, State :: #state{}) ->
    {noreply, NewState :: #state{}}.
handle_info(Info, State) ->
    io:format("INFO message: ~p~n", [Info]),
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    io:format("tt_store is going down!~n", []),
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_ets(Tid, ParentPid) ->
    case ets:info(Tid) of
        undefined ->
            ets:new(Tid, [bag, public, named_table]),
            ets:give_away(Tid, ParentPid, 'take care of my ETS, dude!');
        _TableExists ->
            ok
    end.

-spec insert_data(Table :: ets:tab(), Record :: {Title :: string, StartTime :: calendar:datetime(), EndTime :: calendar:datetime(), Where :: string}) ->
    true.
insert_data(Table, Record) ->
    {Title, StartTime, EndTime, Where} = Record,
    ets:insert(Table,
        #talk{
            title = Title,
            start_time = StartTime,
            end_time = EndTime,
            location = Where
        }).

-spec get_records(Table :: ets:tab()) -> [#talk{}].
get_records(Table) ->
    ets:tab2list(Table).

-spec find_records_by_name(Table :: ets:tab(), Name :: string) -> [#talk{}].
find_records_by_name(Table, Name) ->
    MatchSpec = ets:fun2ms(fun(N = #talk{title = Na}) when Na == Name -> N end),
    ets:select(Table, MatchSpec).

-spec find_records_by_time(Table :: ets:tab(), Time :: {calendar:datetime(), calendar:datetime()}) -> [#talk{}].
find_records_by_time(Table, Time) ->
    {StartTime, EndTime} = Time,
    MatchSpec = ets:fun2ms(fun(N = #talk{start_time = Start, end_time = End}) when Start >= StartTime, End =< EndTime -> N end),
    ets:select(Table, MatchSpec).

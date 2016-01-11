%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(tt_scheduler).
-author("Wojciech Pachuta").

-behaviour(gen_server).

%% API
-export([start_link/0, schedule/3, cancel_schedule/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% auxiliary functions
-export([time_keeper_function/4,  prompt_publishing/1]).

-define(SERVER, ?MODULE).

-record(state, {refs :: #{reference() => pid()}}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(schedule(calendar:datetime(), calendar:datetime(), calendar:time()) -> atom()).
schedule(StartTime, EndTime, PublishInterval) ->
  gen_server:call(?SERVER, {schedule, StartTime, EndTime, PublishInterval}).

-spec(cancel_schedule(reference()) -> no_return()).
cancel_schedule(Ref) ->
  gen_server:cast(?SERVER, {cancel_schedule, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, State :: #state{}}.
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{refs = maps:new()}}.

-spec handle_call({schedule, StartTime :: calendar:datetime(), EndTime :: calendar:datetime(), PublishInterval :: calendar:time()},
    From :: {pid(), Tag :: term()}, State :: #state{}) ->
  {reply, reference(), NewState :: #state{}}.
handle_call({schedule, StartTime, EndTime, PublishInterval}, _From, State) ->
  {Ref, NewState} = do_schedule(StartTime, EndTime, PublishInterval, State),
  {reply, Ref, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast({publish, calendar:time()} | {cancel_schedule, reference()}, State :: #state{}) ->
  {noreply, NewState :: #state{}}.
handle_cast({publish, PublishInterval}, State) ->
  do_publish(PublishInterval),
  {noreply, State};
handle_cast({cancel_schedule, Ref}, State) ->
  do_cancel_schedule(Ref, State),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec handle_info({'EXIT', _, {time_keeper_cancelled | time_keeper_finished, reference()}}, State :: #state{}) ->
  {noreply, NewState :: #state{}}.
handle_info({'EXIT', _, {time_keeper_cancelled, Ref}}, State) ->
  {noreply, State#state{refs = maps:remove(Ref, State#state.refs)}};
handle_info({'EXIT', _, {time_keeper_finished, Ref}}, State) ->
  {noreply, State#state{refs = maps:remove(Ref, State#state.refs)}}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(do_publish(calendar:time()) -> no_return()).
do_publish(PublishInterval) ->
  CTime = calendar:local_time(),
  Talks = tt_store:find_by_time(CTime, add_time(CTime, PublishInterval)),
  tt_publisher:publish(Talks).

-spec(do_schedule(calendar:datetime(), calendar:datetime(), calendar:time(), #state{}) -> {reference(), #state{}}).
do_schedule(StartTime, EndTime, PublishInterval, State) ->
  Ref = make_ref(),
  Pid = spawn_link(?MODULE, time_keeper_function, [StartTime, EndTime, PublishInterval, Ref]),
  {Ref, State#state{refs = maps:put(Ref, Pid, State#state.refs)}}.

-spec(do_cancel_schedule(reference(), #state{}) -> #state{}).
do_cancel_schedule(Ref, State) ->
  case maps:get(Ref, State#state.refs) of
    {badkey, _} -> State;
    Pid ->
      exit(Pid, {time_keeper_cancelled, Ref})
  end.

-spec(add_time(calendar:datetime(), calendar:time()) -> calendar:datetime()).
add_time(T1, T2) ->
  S1 = calendar:datetime_to_gregorian_seconds(T1),
  S2 = calendar:time_to_seconds(T2),
  calendar:gregorian_seconds_to_datetime(S1 + S2).

-spec(time_keeper_function(calendar:datetime(), calendar:datetime(), calendar:time(), reference()) -> no_return()).
time_keeper_function(StartTime, EndTime, PublishInterval, Ref) ->
  TimeToSleep = timer:seconds(calendar:datetime_to_gregorian_seconds(StartTime) - calendar:datetime_to_gregorian_seconds(calendar:local_time())),
  timer:sleep(TimeToSleep),

  TimeToRun = timer:seconds(calendar:datetime_to_gregorian_seconds(EndTime) - calendar:datetime_to_gregorian_seconds(calendar:local_time())),
  case timer:exit_after(TimeToRun, {time_keeper_finished, Ref}) of
    {error, Reason} -> exit(self(), Reason);
    _ -> ok
  end,

  timer:apply_interval(timer:seconds(calendar:time_to_seconds(PublishInterval)), ?MODULE, prompt_publishing, [PublishInterval]),
  timer:sleep(infinity).

-spec(prompt_publishing(calendar:time()) -> no_return()).
prompt_publishing(PublishInterval) ->
  gen_server:cast(?SERVER, {publish, PublishInterval}).
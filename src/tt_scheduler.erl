%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(tt_scheduler).
-author("Wojciech Pachuta").

-behaviour(gen_server).

%% API
-export([stop/0, schedule/3, cancel_schedule/1, start_link/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% auxiliary functions
-export([time_keeper_function/4, prompt_action/1]).

-define(SERVER, ?MODULE).

-record(schedule, {start_time :: calendar:datetime(), end_time :: calendar:datetime(), time_before_talk :: calendar:time(), pid :: pid()}).
-record(state, {refs :: #{reference() => #schedule{}}, action_interval :: calendar:time(), time_window :: calendar:time()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link(ActionInterval :: calendar:time(), TimeWindow :: calendar:time()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ActionInterval, TimeWindow) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ActionInterval, TimeWindow], []).
stop() ->
    gen_server:stop(?SERVER).

-spec(schedule(calendar:datetime(), calendar:datetime(), calendar:time()) -> reference()).
schedule(StartTime, EndTime, TimeBeforeTalk) ->
  gen_server:call(?SERVER, {schedule, StartTime, EndTime, TimeBeforeTalk}).

-spec(cancel_schedule(reference()) -> no_return()).
cancel_schedule(Ref) ->
  gen_server:cast(?SERVER, {cancel_schedule, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([calendar:time()]) -> {ok, State :: #state{}}.
init([ActionInterval, TimeWindow]) ->
  process_flag(trap_exit, true),
  {ok, #state{refs = maps:new(), action_interval = ActionInterval, time_window = TimeWindow}}.

-spec handle_call({schedule, StartTime :: calendar:datetime(), EndTime :: calendar:datetime(), PublishInterval :: calendar:time()},
    From :: {pid(), Tag :: term()}, State :: #state{}) ->
  {reply, reference(), NewState :: #state{}}.
handle_call({schedule, StartTime, EndTime, TimeBeforeTalk}, _From, State) ->
  {Ref, NewState} = do_schedule(StartTime, EndTime, TimeBeforeTalk, State),
  {reply, Ref, NewState};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

-spec handle_cast({action, calendar:time()} | {cancel_schedule, reference()}, State :: #state{}) ->
  {noreply, NewState :: #state{}}.
handle_cast({action, Ref}, State) ->
  do_action(Ref, State),
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
  log_finished_schedule(Ref, State#state.refs),
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

-spec(do_action(reference(), #state{}) -> no_return()).
do_action(Ref, State) ->
  case maps:find(Ref, State#state.refs) of
    {ok, #schedule{time_before_talk = TimeBeforeTalk}} ->
      CTime = calendar:local_time(),
      STime = add_time(CTime, TimeBeforeTalk),
      ETime = add_time(STime, State#state.time_window),
      Talks = tt_store:find_by_time_unpublished(STime, ETime),
      tt_store:mark_published(Talks),
      tt_publisher:publish(Talks);
    _ -> ok
  end.

-spec(do_schedule(calendar:datetime(), calendar:datetime(), calendar:ti1me(), #state{}) -> {reference(), #state{}}).
do_schedule(StartTime, EndTime, TimeBeforeTalk, State) ->
  Ref = make_ref(),
  Pid = spawn_link(?MODULE, time_keeper_function, [StartTime, EndTime, State#state.action_interval, Ref]),
  log_msg("New schedule", StartTime, EndTime, TimeBeforeTalk),
  {Ref, State#state{refs = maps:put(
    Ref, #schedule{start_time = StartTime, end_time = EndTime, time_before_talk = TimeBeforeTalk, pid = Pid},
    State#state.refs)}}.

-spec(do_cancel_schedule(reference(), #state{}) -> #state{}).
do_cancel_schedule(Ref, State) ->
  case maps:find(Ref, State#state.refs) of
    {ok, #schedule{pid = Pid, start_time = StartTime, end_time = EndTime, time_before_talk = TimeBeforeTalk}} ->
      log_msg("Cancelled schedule", StartTime, EndTime, TimeBeforeTalk),
      exit(Pid, {time_keeper_cancelled, Ref});
    _ -> State
  end.

-spec(add_time(calendar:datetime(), calendar:time()) -> calendar:datetime()).
add_time(T1, T2) ->
  S1 = calendar:datetime_to_gregorian_seconds(T1),
  S2 = calendar:time_to_seconds(T2),
  calendar:gregorian_seconds_to_datetime(S1 + S2).

-spec(time_keeper_function(calendar:datetime(), calendar:datetime(), calendar:time(), reference()) -> no_return()).
time_keeper_function(StartTime, EndTime, ActionInterval, Ref) ->
  TimeToSleep = timer:seconds(calendar:datetime_to_gregorian_seconds(StartTime) - calendar:datetime_to_gregorian_seconds(calendar:local_time())),
    case TimeToSleep > 0 of
        true ->
            timer:sleep(TimeToSleep);
        false ->
            tt_loger:log(info, "The schedule start time has passed; starting right away")
    end,

  TimeToRun = timer:seconds(calendar:datetime_to_gregorian_seconds(EndTime) - calendar:datetime_to_gregorian_seconds(calendar:local_time()) + 1),
  case timer:exit_after(TimeToRun, {time_keeper_finished, Ref}) of
    {error, Reason} -> exit(self(), Reason);
    _ -> ok
  end,

  timer:apply_interval(timer:seconds(calendar:time_to_seconds(ActionInterval)), ?MODULE, prompt_action, [Ref]),
  timer:sleep(infinity).

-spec(prompt_action(reference()) -> no_return()).
prompt_action(Ref) ->
  gen_server:cast(?SERVER, {action, Ref}).

-spec log_finished_schedule(reference(), #{}) -> no_return().
log_finished_schedule(Ref, Refs) ->
  case maps:find(Ref, Refs) of
    {ok, #schedule{start_time = StartTime, end_time = EndTime, time_before_talk = TimeBeforeTalk}} ->
      log_msg("Finished schedule", StartTime, EndTime, TimeBeforeTalk), ok;
    _ -> ok
  end.

log_msg(Msg, StartTime, EndTime, TimeBeforeTalk) ->
  M = io_lib:format("~s. Start time: ~s, End time: ~s, Time between talks: ~s", [
    Msg,
    tt_publisher:datetime_to_string(StartTime),
    tt_publisher:datetime_to_string(EndTime),
    tt_publisher:time_to_string(TimeBeforeTalk)
  ]),
  tt_loger:log(info, M).

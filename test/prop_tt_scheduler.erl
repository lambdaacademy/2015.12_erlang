-module(prop_tt_scheduler).

-export([initial_state/0,
         command/1,
         precondition/2,
         postcondition/3,
         next_state/3]).

-include_lib("proper/include/proper.hrl").

-record(state, {schedules = [] :: [reference()]}).

-define(SERVER, tt_scheduler).
-define(MOCK, [tt_store, tt_publisher]).

%%%===================================================================
%%% Properties
%%%===================================================================

prop_sanity_check() ->
    proper:numtests(10, true).

prop_server_alive_when_schedule_is_over() ->
    proper:numtests(
      10,
      ?FORALL([Start, Stop, Interval], schedule_timing(),
              ?TRAPEXIT(begin
                            setup(),
                            {ok, Pid} = ?SERVER:start_link(
                                           _ActionInterval = {0,0,1},
                                           _TimeWindow = {0,0,1}),
                            
                            %% WHEN
                            tt_scheduler:schedule(Start, Stop, Interval),
                            meck:wait(tt_publisher, publish, '_', 10000),
                            
                            %% THEN
                            Res = is_process_alive(Pid),
                            ?SERVER:stop(),
                            Res
                        end))).


prop_scheduler_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   setup(),
                   ?SERVER:start_link(_ActionInterval = {0,0,1},
                                      _TimeWindow = {0,0,1}),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   ?SERVER:stop(),
                   ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w~n",
                                       [History, State, Result]),
                             Result =:= ok)
               end)).

%%%===================================================================
%%% Callbacks
%%%===================================================================

initial_state() ->
    #state{}.

command(S) ->
    Schedules = (S#state.schedules =/= []),
    oneof([{call, ?SERVER, schedule, schedule_timing()}] ++
              [{call, ?SERVER, cancel_schedule, [schedule_ref(S)]}
               || Schedules]).

precondition(S, {call, _, cancel_schedule, [Ref]}) ->
    lists:member(Ref, S#state.schedules);
precondition(_, _) -> true.

postcondition(_S, {call, _, schedule, _}, V) ->
    is_reference(V) andalso is_process_alive(whereis(?SERVER));
postcondition(_, _, _) ->
    true.


next_state(S, V, {call, _, schedule, _}) ->
    S#state{schedules = [V|S#state.schedules]};
next_state(S, _V, {call, _, cancel_schedule, [Ref]}) ->
    S#state{schedules = lists:delete(Ref, S#state.schedules)}.

schedule_timing() ->
    ?LET({StartAfterS, StopAfterS, Interval},
         {integer(1,2), integer(2,5), integer(1,2)},
         tuple_to_list(start_stop_interval(StartAfterS,
                                           StopAfterS,
                                           Interval))).

schedule_ref(#state{schedules = Schedules}) ->
    elements(Schedules).

%%%===================================================================
%%% Internal functions
%%%===================================================================

setup() ->
    meck:unload(),
    ok = meck:new(?MOCK),
    [meck:expect(tt_publisher, F, A, ok)
     || {F,A} <- [{publish, 1},
                  {datetime_to_string, 1},
                  {time_to_string, 1}]],
    [meck:expect(tt_store, F, A, ok)
     || {F,A} <- [{find_by_time, 2},
                  {find_by_time_unpublished, 2},
                  {mark_published, 1}]].

-spec(start_stop_interval(StartAfterSeconds :: non_neg_integer(),
                          StopAfterSeconds :: non_neg_integer(),
                          IntervalSeconds :: non_neg_integer()) ->
             {Datetime, Datetime, Interval}
                 when Datetime :: calendar:datetime(),
                      Interval :: calendar:time()).
start_stop_interval(StartAfterSeconds,
                    StopAfterSeconds,
                    IntervalSeconds) ->
    Start = increment_datetime(calendar:local_time(),
                               StartAfterSeconds),
    Stop = increment_datetime(Start, StopAfterSeconds),
    Interval = calendar:seconds_to_time(IntervalSeconds),
    {Start, Stop, Interval}.

-spec(increment_datetime(calendar:datetime(), non_neg_integer()) ->
             calendar:datetime()).
increment_datetime(DateTime, Seconds) ->
    DTSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(DTSeconds + Seconds).


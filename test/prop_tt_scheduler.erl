-module(prop_tt_scheduler).

-include_lib("proper/include/proper.hrl").


%%%===================================================================
%%% Properties
%%%===================================================================

prop_sanity_check() ->
    proper:numtests(10, true).

prop_server_alive_when_schedule_is_over() ->
    proper:numtests(
      1,
      ?TRAPEXIT(begin
                    %% GIVEN
                    Pid = start_scheduler(),
                    {Start, Stop, Interval} = start_stop_interval(
                                                0,
                                                StopAfterSeconds = 2,
                                                1),

                    %% WHEN
                    tt_scheduler:schedule(Start, Stop, Interval),
                    timer:sleep(timer:seconds(StopAfterSeconds + 1)),

                    %% THEN
                    true = is_process_alive(Pid)
                end)).

prop_server_alive_when_schedule_cancelled() ->
    proper:numtests(
      1,
      ?TRAPEXIT(begin
                    %% GIVEN
                    Pid = start_scheduler(),
                    {Start, Stop, Interval} = start_stop_interval(
                                                0,
                                                StopAfterSeconds = 2,
                                                1),

                    %% WHEN
                    Ref = tt_scheduler:schedule(Start, Stop, Interval),
                    tt_scheduler:cancel_schedule(Ref),
                    timer:sleep(timer:seconds(StopAfterSeconds + 1)),
                    
                    %% THEN
                    true = is_process_alive(Pid)
                end)).


prop_server_calls_tt_store() ->
    proper:numtests(
      1,
      ?TRAPEXIT(begin
                    %% GIVEN
                    meck:new(tt_store, [passthrough]),
                    Pid = start_scheduler(),
                    {Start, Stop, Interval} =
                        start_stop_interval(0,
                                            StopAfterSeconds = 3,
                                            IntervalSeconds = 1),

                    %% WHEN
                    tt_scheduler:schedule(Start, Stop, Interval),
                    Times = at_least_times(StopAfterSeconds,
                                           IntervalSeconds),
                    
                    %% THEN
                    meck:wait(Times, tt_store, find_by_time, '_',
                              timer:seconds(StopAfterSeconds + 1)),
                    meck:unload(tt_store),
                    is_process_alive(Pid)
                end)).

prop_server_calls_tt_publisher() ->
    proper:numtests(
      1,
      ?TRAPEXIT(begin
                    %% GIVEN
                    meck:new(tt_publisher, [passthrough]),
                    Pid = start_scheduler(),
                    {Start, Stop, Interval} =
                        start_stop_interval(0,
                                            StopAfterSeconds = 3,
                                            IntervalSeconds = 1),

                    %% WHEN
                    tt_scheduler:schedule(Start, Stop, Interval),
                    Times = at_least_times(StopAfterSeconds,
                                           IntervalSeconds),

                    %% THEN
                    meck:wait(Times, tt_publisher, publish, 1,
                              timer:seconds(StopAfterSeconds + 1)),
                    meck:unload(tt_publisher),
                    is_process_alive(Pid)
                end)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

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

-spec(start_scheduler() -> pid()).
start_scheduler() ->
    catch gen_server:stop(tt_scheduler),
    {ok, Pid} = tt_scheduler:start_link(),
    Pid.

-spec(increment_datetime(calendar:datetime(), non_neg_integer()) ->
             calendar:datetime()).
increment_datetime(DateTime, Seconds) ->
    DTSeconds = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(DTSeconds + Seconds).

-spec(at_least_times(non_neg_integer(), non_neg_integer()) ->
             non_neg_integer()).
at_least_times(StopAfterSeconds, IntervalSeconds) ->
    (StopAfterSeconds div IntervalSeconds) - 1.


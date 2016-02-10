%%%-------------------------------------------------------------------
%% @doc talks_tweeter public API
%% @end
%%%-------------------------------------------------------------------

-module('talks_tweeter_app').

-behaviour(application).

-define(REQ_OPTS, [screen_name,
                   consumer_key,
                   consumer_secret,
                   access_token,
                   access_token_secret]).

%% Application callbacks
-export([start/2
        ,stop/1
        ,run/0]).

-define(SECONDS_PER_DAY, 24*60*60).
-define(TALKS_FILE, filename:join([code:priv_dir(talks_tweeter), "talks.csv"])).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    validate_config(),
    Ret = talks_tweeter_sup:start_link(),
    tt_loger:log(info,
                 io_lib:format("Starting app with config: ~p~n",
                               [application:get_all_env(talks_tweeter)])),
    Ret.

run() ->
    StartDateTime = {StartDate, _StartTime} =
        application:get_env(talks_tweeter,
                            schedule_start_time,
                            calendar:local_time()),
    EndDateTime = application:get_env(talks_tweeter,
                                      schedule_end_time,
                                      two_days_later(calendar:local_time())),
    ok = tt_importer:import_csv_file(?TALKS_FILE, StartDate),
    tt_scheduler:schedule(StartDateTime, EndDateTime,
                          application:get_env(talks_tweeter,
                                              before_talk,
                                              {0,15,0})).

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

validate_config() ->
    [begin
         undefined == application:get_env(etweet, O, undefined)
             andalso throw({required_option_not_set, O})
     end || O <- ?REQ_OPTS].

two_days_later(StartDateTime) ->
    CL=calendar,
    CL:gregorian_seconds_to_datetime(
      CL:datetime_to_gregorian_seconds(StartDateTime) +
          ?SECONDS_PER_DAY*2).

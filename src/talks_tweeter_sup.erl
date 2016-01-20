%%%-------------------------------------------------------------------
%% @doc talks_tweeter top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('talks_tweeter_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    InitLogLevel = application:get_env(talks_tweeter, log_level, info),
    LoggerWorker = #{id => tt_loger,
                    start => {tt_loger, start_link, [InitLogLevel]},
                    restart => permanent,
                    type => worker,
                    modules => [tt_loger]
                   },
    StoreWorker = #{id => tt_store,
                    start => {tt_store, start_link, []},
                    restart => permanent,
                    type => worker,
                    modules => [tt_store]
                   },
    ImporterWorker = #{id => tt_importer,
                       start => {tt_importer, start_link, []},
                       restart => permanent,
                       type => worker,
                       modules => [tt_importer]
                      },
    SchedulerWorker = #{id => tt_scheduler,
                        start => {tt_scheduler, start_link, []},
                        restart => permanent,
                        type => worker,
                        modules => [tt_scheduler]
                       },
    {ok, { {one_for_one, 5, 10}, [LoggerWorker, StoreWorker, ImporterWorker, SchedulerWorker]} }.

%%====================================================================
%% Internal functions
%%====================================================================

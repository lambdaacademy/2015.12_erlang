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
    LogerWorker = #{id => tt_loger,
                    start => {tt_loger, start_link, [InitLogLevel]},
                    restart => permanent,
                    type => worker,
                    modules => [tt_loger]
                   },
    {ok, { {one_for_one, 5, 10}, [LogerWorker]} }.

%%====================================================================
%% Internal functions
%%====================================================================

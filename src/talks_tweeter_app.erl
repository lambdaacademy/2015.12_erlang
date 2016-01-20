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
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    validate_config(),
  talks_tweeter_sup:start_link().

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

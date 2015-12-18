-module(tt_loger).

-behaviour(gen_server).

%% API
-export([start_link/1,
         log/2,
         change_level/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {level :: atom}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(InitLogLevel) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [InitLogLevel], []).

-spec log(Level :: atom(), Messsag :: list()) -> ok.
log(Level, Message) ->
    gen_server:cast(?SERVER, {log, Level, Message}).

change_level(Level) ->
    gen_server:call(?SERVER, {level_change, Level}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([LogLevel]) ->
    {ok, #state{level = LogLevel}}.


handle_call({level_change, NewLevel}, _From, State)
  when NewLevel =:= debug orelse NewLevel =:= info ->
    {reply, ok, State#state{level = NewLevel}};
handle_call(_Request, _From, State) ->
    {reply, unknown_log_level, State}.

%% Log all messages when the level is 'debug'
handle_cast({log, Level, Msg}, State = #state{level = debug}) ->
    do_log(Level, Msg),
    {noreply, State};
%% Log only 'info' messages when level is 'info'
handle_cast({log, info = Level, Msg}, State = #state{level = info}) ->
    do_log(Level, Msg),
    {noreply, State};
%% Do not log otherwise
handle_cast(_, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_log(Level, Msg) ->
    io:format("[~p] ~s", [Level, Msg]).

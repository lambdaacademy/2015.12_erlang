-module(tt_store).

-record(talk, {title :: string(), start_time :: calendar:datetime(), end_time :: calendar:datetime(), location :: string()}).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add/3,
         find_by_time/2,
         list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(add(Title :: string, StartTime :: calendar:datetime(), EndTime :: calendar:datetime()) -> ok).
add(Title, StartTime, EndTime) ->
    gen_server:call(?SERVER, {add, Title, StartTime, EndTime}).

-spec(find_by_time(StartTime :: calendar:datetime(), EndTime :: calendar:datetime()) -> [#talk{}]).
find_by_time(StartTime, EndTime) ->
    gen_server:call(?SERVER, {log, StartTime, EndTime}).

list() ->
    gen_server:call(?SERVER, {list}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
    {ok, []}.


handle_call({list}, _From, State) ->
    {reply, ok};
handle_call({add, Title, StartTime, EndTime}, _From, State) ->
    {reply, ok};
handle_call({log, StartTime, EndTime}, _From, State) ->
    {reply, ok, []};
handle_call(_Request, _From, State) ->
    {reply, unknown_log_level, State}.

handle_cast(_Request, State) ->
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

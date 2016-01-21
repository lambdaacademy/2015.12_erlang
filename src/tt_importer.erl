-module(tt_importer).
-author("Adam Szlachta").

-behaviour(gen_server).

%% API
-export([start_link/0, import_file/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, talks).

% -record(talk,  {title :: string(), start_time :: calendar:datetime(), end_time :: calendar:datetime(), location :: string()}).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec import_file(Filename :: string) -> ok.
import_file(Filename) ->
    gen_server:call(?SERVER, {import_file, Filename}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, State :: atom()}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, state}.

-spec handle_call({import_file, Filename :: string}, From :: {pid(), Tag :: term()}, State :: atom()) ->
    {reply, ok, NewState :: atom()}.
handle_call({import_file, Filename}, _From, State) ->
    import_file_to_store(Filename),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

-spec handle_cast(any(), State :: atom()) -> {noreply, NewState :: atom()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info({'EXIT', _, {time_keeper_cancelled | time_keeper_finished, reference()}}, State :: atom()) ->
    {noreply, NewState :: atom()}.
handle_info(Info, State) ->
    io:format("INFO message: ~p~n", [Info]),
    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: atom()) -> term()).
terminate(_Reason, _State) ->
    io:format("tt_store is going down!~n", []),
    ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: atom(), Extra :: term()) ->
    {ok, NewState :: atom()} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec import_file_to_store(Filename :: string) -> ok.
import_file_to_store(Filename) ->
    {ok, Talks} = file:consult(Filename),
    lists:foreach(fun({Title, StartTime, EndTime, Where}) -> tt_store:add(Title, StartTime, EndTime, Where) end, Talks),
    ok.

% tt_importer:import_file("talks.txt").

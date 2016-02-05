-module(tt_importer).
-author("Adam Szlachta").

-behaviour(gen_server).

%% API
-export([start_link/0, import_file/1, import_csv_file/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, talks).
-define(SECONDS_PER_DAY, 24*60*60).

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

-spec import_csv_file(Filename :: filename:all(),
                      StareDate :: calendar:date()) ->
                             ok | {error, Reason :: term()}.
import_csv_file(Filename, StartDate) ->
    case filelib:is_file(Filename) of
        true ->
            gen_server:call(?SERVER,
                            {import_csv_file, Filename, StartDate});
        false ->
            {error, file_not_exists}
    end.

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
handle_call({import_csv_file, Filename, StartDate}, _From, State) ->
    import_csv_file2(Filename, StartDate),
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

import_csv_file2(Filename, StartDate) ->
    {ok, IoDevice} = file:open(Filename, [read]),
    {ok, Cnt} = ecsv:process_csv_file_with(
                  IoDevice,
                  fun({newline, Line}, Cnt) ->
                          import_csv_line(Line, StartDate),
                          Cnt + 1;
                     ({eof}, Cnt) ->
                          Cnt
                  end,
                  0),
    tt_loger:log(info, io_lib:format("Imported ~p CSV entriees", [Cnt])),
    ok = file:close(IoDevice).

import_csv_line([Title, Room, Day, MinsFrom7Am], StartDate) ->
    StartDateTime = start_datetime(StartDate, Day, MinsFrom7Am),
    EndDateTime = end_datetime(StartDateTime, Title, Room),
    tt_store:add(Title, StartDateTime, EndDateTime, location(Room)).


location("room 1") ->
    "Aula średnia A";
location("room 2") ->
    "Aula średnia B";
location(_) ->
    "Aula mała".

start_datetime(StartDate, Day, MinsFrom7Am) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds({StartDate, {7,0,0}})
      + (list_to_integer(Day)-1)*?SECONDS_PER_DAY
      + list_to_integer(MinsFrom7Am)*60).

end_datetime(StartDateTime, Title, Room) ->
    calendar:gregorian_seconds_to_datetime(
      calendar:datetime_to_gregorian_seconds(StartDateTime)
      + calendar:time_to_seconds(talk_duration(Title, Room))).

talk_duration(_, "Resarch Track") ->
    {0,25,0};
talk_duration(Title, _) ->
    case string:str(Title, "Keynote") of
        0 ->
            {0,45,0};
        _ ->
            {1,0,0}
    end.

    

            

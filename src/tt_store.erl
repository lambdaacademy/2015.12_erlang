-module(tt_store).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include_lib("stdlib/include/ms_transform.hrl").
%% API
-export([start_link/0, add/4, find_by_time/2, list/0, find_by_name/1]).

%% Supervisor callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% DB record
-record(talk, {title :: string(), startTime :: {Hour, Minute, Second}, endTime :: {Hour, Minute, Second}, location :: any()}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->     
    gen_server:start_link({local, ?SERVER},?MODULE, [], []).

add(Title, StartTime, EndTime, Where) -> 
    gen_server:cast(?SERVER, {add, {Title, StartTime, EndTime, Where}}),
    ok.

find_by_time(StartTime, EndTime) -> 
    gen_server:call(?SERVER, {find_time, {StartTime, EndTime}}).

list() -> 
    gen_server:cast(?SERVER, list).

find_by_name(Name) ->
    gen_server:call(?SERVER, {find_name, Name}).
%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) -> 
    ets:new(talks, [set, named_table]),
    process_flag(trap_exit, true),
    {ok, talks}.

handle_call({find_time, {StartTime, EndTime}}, _From, State) -> 
    Records = find_records_by_time({StartTime, EndTime}),    
    {reply, Records, talks};

handle_call({find_name, Name}, _From, State) -> 
    Records = find_records_by_name(Name),
    {reply, Records, talks}.

handle_cast({add, {Title, StartTime, EndTime, Where}}, State) -> 
    insert_data({Title, StartTime, EndTime, Where}),
    {noreply, talks};

handle_cast(list, State) -> 
    print_records(),
    {noreply, talks}.

handle_info(Info, State) -> io:format("INFO message: ~p~n", [Info]),
                            {noreply, State}.

terminate(Reason, State) -> io:format("tt_store is going down!~n", []),
                            ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
insert_data(Record) -> 
    {Title, StartTime, EndTime, Where} = Record,   
    ets:insert(talks, 
     #talk{
        title = Title,
        startTime = StartTime,
        endTime = EndTime,
        location = Where                                  
            }).

print_records() ->
    Records = ets:match(talks, '$1'),
    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, Records).

find_records_by_name(Name) -> 
    MatchSpec = ets:fun2ms(fun(N = #talk{title=Na}) when Na == Name -> N end),
    Res = ets:select(talks, MatchSpec),
    Res.

find_records_by_time(Time) -> 
    {StartTime, EndTime} = Time,
    MatchSpec = ets:fun2ms(fun(N = #talk{startTime = S, endTime = E}) when S == StartTime; E == endTime -> N end),
    Res = ets:select(talks, MatchSpec),
    Res.

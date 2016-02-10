-module(tt_publisher).
-author("Slawomir Kowalski").

%% API
-export([publish/1, start_link/0]).
%% gen server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% UTIL API
-export([publish_inner/2]).
%% utils
-export([datetime_to_string/1, time_to_string/1]).


-behaviour(gen_server).

-define(SERVER, ?MODULE).

-ifdef(TWEET_MOCK).
-define(TWEET(Talk), io:format("~s~n", [Talk])).
-else.
-define(TWEET(Talk), etweet:tweet(Talk)).
-endif.

-include("talks_tweeter.hrl").
-record(state, {}).

%% API
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(publish(Talks :: [#talk{}]) -> no_return()).
publish(Talks) ->
  publish_inner(?SERVER, Talks).


%% GEN SERVERS
-spec init([]) -> {ok, state}.
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

-spec handle_call(_Request, _From, State :: #state{}) -> {reply, ok, #state{}}.
handle_call(_, _, State) ->
  {reply, ok, State}.

-spec(handle_cast({publish_small, Talk :: #talk{}}, State :: #state{}) -> no_return()).
handle_cast({publish_small, Talk}, State) ->
  ?TWEET(Talk),
  {noreply, State}.

-spec handle_info(_, State :: #state{}) -> {noreply, #state{}}.
handle_info(_, State) ->
  {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term()).
terminate(_, _) ->
  ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_, State, _) ->
  {ok, State}.


%% UTILS
-spec(publish_inner(Pid :: pid(), Talks :: [#talk{}]) -> no_return()).
publish_inner(Pid, Talks) ->
  lists:foreach(fun(Talk) -> publish_single_talk(Pid, Talk) end, Talks).

-spec(publish_single_talk(Pid :: pid(), Talk :: #talk{}) -> no_return()).
publish_single_talk(Pid, Talk) ->
  ValueToPublish = readable_talk(Talk),
  gen_server:cast(Pid, {publish_small, ValueToPublish}).

-spec(readable_talk(Talk :: #talk{}) -> string()).
readable_talk(Talk) ->
    Formatted0 = io_lib:format("~s, ~s - ~s, ~s",
                               [Talk#talk.title,
                                datetime_to_time_string_without_secnods(Talk#talk.start_time),
                                datetime_to_time_string_without_secnods(Talk#talk.end_time),
                                Talk#talk.location]),
    Formatted1 = lists:flatten(Formatted0),
    tt_loger:log(info, io_lib:format("Publishing: ~s", [Formatted1])),
    Formatted1.

datetime_to_time_string_without_secnods({_, {H, M, _}}) ->
    lists:flatten(io_lib:format('~2..0b:~2..0b', [H, M])).

-spec(datetime_to_string(calendar:datetime()) -> string()).
datetime_to_string({{Year, Month, Day}, {Hour, Min, Sec}}) ->
  %io_lib:format('~2..0b.~2..0b.~4..0b ~2..0b:~2..0b:~2..0b', [Day, Month, Year, Hour, Min, Sec]).
  lists:concat([Day, ".", Month, ".", Year, " ", Hour, ":", Min, ":", Sec]).

-spec time_to_string(calendar:date()) -> string().
time_to_string({Hour, Min, Sec}) ->
  lists:concat([Hour, ":", Min, ":", Sec]).

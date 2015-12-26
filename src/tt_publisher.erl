-module(tt_publisher).

-export([publish/1]).

-record(talk, {title :: string(), start_time :: calendar:datetime(), end_time :: calendar:datetime(), location :: string()}).

-spec(publish([#talk{}]) -> no_return()).
publish(Talks) ->
  lists:foreach(fun(Talk) -> io:format("~s, ~s, ~s, ~s~n", [Talk#talk.title, datetime_to_string(Talk#talk.start_time), datetime_to_string(Talk#talk.end_time), Talk#talk.location]) end, Talks).

-spec(datetime_to_string(calendar:datetime()) -> string()).
datetime_to_string({{Year, Month, Day}, {Hour, Min, Sec}}) ->
  io_lib:format('~2..0b.~2..0b.~4..0b ~2..0b:~2..0b:~2..0b', [Day, Month, Year, Hour, Min, Sec]).
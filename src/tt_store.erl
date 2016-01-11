-module(tt_store).

-export([find_by_time/2]).

-record(talk, {title :: string(), start_time :: calendar:datetime(), end_time :: calendar:datetime(), location :: string()}).

-spec(find_by_time(calendar:datetime(), calendar:datetime()) -> [#talk{}]).
find_by_time(_StartTime, _EndTime) ->
  [#talk{title = "Title", start_time = calendar:local_time(), end_time = calendar:local_time(), location =  "Location"}].

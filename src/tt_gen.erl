%%%-------------------------------------------------------------------
%%% @author Wojciech Pachuta
%%%-------------------------------------------------------------------
-module(tt_gen).
-author("Wojciech Pachuta").

%% API
-export([gen_talks/4, gen_talks_now/3]).

-spec gen_talks(calendar:datetime(), integer(), integer(), filename:all()) -> true.
gen_talks(StartTime, Num, IntervalSec, OutFilename) ->
  file:write_file(OutFilename, "", [write]),
  gen_talks_int(StartTime, Num, IntervalSec, OutFilename, 0).

-spec gen_talks_now(integer(), integer(), filename:all()) -> true.
gen_talks_now(Num, IntervalSec, OutFilename) ->
  gen_talks(calendar:local_time(), Num, IntervalSec, OutFilename).

gen_talks_int(_StartTime, 0, _IntervalSec, _OutFileName, _Cnt) -> ok;
gen_talks_int(StartTime, Num, IntervalSec, OutFileName, Cnt) ->
  Talk = {lists:flatten(io_lib:format("Talk~p", [Cnt])), StartTime, time_inc(StartTime, 10, 0), lists:flatten(io_lib:format("Location~p", [Cnt]))},
  file:write_file(OutFileName, io_lib:format("~p.~n", [Talk]), [append]),
  gen_talks_int(time_inc(StartTime, 0, IntervalSec), Num - 1, IntervalSec, OutFileName, Cnt + 1).

time_inc(Time, Min, Sec) ->
  S1 = calendar:datetime_to_gregorian_seconds(Time),
  calendar:gregorian_seconds_to_datetime(S1 + 60 * Min + Sec).

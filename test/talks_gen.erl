-module(talks_gen).

-export([gen_talk/1,
         gen_talks/3]).

-include_lib("proper/include/proper.hrl").

-define(PREFIXES, ["School of ",
                   "How to meet your ",
                   "Where is ",
                   "Be my ",
                   "I love ",
                   "I hate ",
                   "Joe, do you know ",
                   "Keynote: ",
                   "Bad news: ",
                   "Who are you, "]).

-define(TITLES, ["Erlang",
                 "Elixir",
                 "Cooking",
                 "Haskell",
                 "Scala",
                 "teacher?",
                 "wife?",
                 "Keeping distance",
                 "Alice?"]).
-define(LOCALTIONS, ["ESL Office Krakow",
                     "ESL Office London",
                     "ESL Office Budapest",
                     "ESL Office Sweden",
                     "ESL Office San Franciso"]).

%% API

-spec gen_talks(calendar:datetime(), integer(), filename:all()) -> true.
gen_talks(StartTime, Num, OutFilename) ->
    proper:quickcheck(
      ?FORALL({Title, {T1, T2}, Location},
                              gen_talk(StartTime),
                              begin
                                  Talk = {Title, T1, T2, Location},
                                  io:format("~p~n", [Talk]),
                                  ok = file:write_file(
                                         OutFilename,
                                         io_lib:format("~p.~n", [Talk]),
                                         [append]),
                                  true
                              end), {numtests, Num}).


%% talks_gen:gen_talks(calendar:local_time(), 10, "ala.txt").
gen_talk(StartTime) ->
    {gen_title(), gen_times(StartTime), gen_localtion()}.

%% Helpers

gen_title() ->
    ?LET({Prefix, Suffix},
         {elements(?PREFIXES), elements(?TITLES)},
         Prefix ++ Suffix).

gen_localtion() ->
    elements(?LOCALTIONS).

gen_times(T) ->
    ?LET({AddMinutes1, AddMinutes2}, {integer(1, 60), integer(1, 60)},
         begin
             S1 = calendar:datetime_to_gregorian_seconds(T),
             S2 = AddMinutes1 * 60,
             S3 = AddMinutes2 * 60,
             {calendar:gregorian_seconds_to_datetime(S1 + S2),
              calendar:gregorian_seconds_to_datetime(S1 + S2 + S3)}
         end).

-module(talks_gen).

-export([gen_talks/3]).

-include_lib("proper/include/proper.hrl").

-define(TITLES, ["School of Erlang",
                 "School of Elixir",
                 "School of Cooking",
                 "How to be a teacher?",
                 "How to meet your wife?",
                 "Keeping distance"]).
-define(LOCALTIONS, ["ESL Office Krakow",
                    "ESL Office London",
                    "ESL Office Budapest",
                    "ESL Office Sweden",
                    "ESL Office San Franciso"]).

-spec gen_talks(calendar:datetime(), integer(), filename:all()) -> true.
gen_talks(StartTime, Num, OutFilename) ->
    proper:quickcheck(
      proper:numtests(Num,
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
                              end))).

%% talks_gen:gen_talks(calendar:local_time(), 10, "ala.txt").
gen_talk(StartTime) ->
    {gen_title(), gen_times(StartTime), gen_localtion()}.

gen_title() ->
    elements(?TITLES).

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
    
    











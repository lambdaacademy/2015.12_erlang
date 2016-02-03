-module(talks_gen).

-export([gen_talk/1,
         gen_talk2/0,
         gen_talks/3,
         gen_talks2/2]).

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
-define(LOCATIONS2, ["room 1", "room 2", "room 3", "Research Track"]).
-define(FIVE_PM_IN_MUNTES_FROM7, 10 * 60).

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

gen_talks2(OutFilename, Num) ->
    file:delete(OutFilename),
    proper:quickcheck(
      ?FORALL(Talk,
              gen_talk2(),
              begin
                  Title = element(1, Talk),
                  Format = case
                               string:chr(Title, $,) +
                               string:chr(Title, $:)
                           of
                               0 ->
                                   "~s,~s,~p,~p~n";
                               _ ->
                                   "~p,~s,~p,~p~n"
                           end,
                  T = io_lib:format(Format, tuple_to_list(Talk)),
                  case put(Title,0) of
                      undefined ->
                          ok == file:write_file(OutFilename, T, [append]);
                      _ ->
                          true
                  end
              end), [{numtests, Num}, quiet]).

%% talks_gen:gen_talks(calendar:local_time(), 10, "ala.txt").
gen_talk(StartTime) ->
    {gen_title(), gen_times(StartTime), gen_localtion()}.

gen_talk2() ->
    {gen_title(), gen_location2(), gen_day(), gen_minutes_from_7_am()}.

%% Helpers

gen_title() ->
    ?LET({Prefix, Suffix},
         {elements(?PREFIXES), elements(?TITLES)},
         Prefix ++ Suffix).

gen_localtion() ->
    elements(?LOCALTIONS).

gen_location2() ->
    elements(?LOCATIONS2).

gen_day() ->
    integer(1,2).

gen_minutes_from_7_am() ->
    %% interwały 25 minut, 45 minut, 60 minut
    ?SUCHTHAT(Time,
              ?LET({I, X},
                   {gen_talk_interval(), integer(1, 20)},
                   60 + I+X),
              Time < ?FIVE_PM_IN_MUNTES_FROM7).


gen_talk_interval() ->
    frequency([{1, 60},
               {4, 25},
               {4, 45}]).
    

gen_times(T) ->
    ?LET({AddMinutes1, AddMinutes2}, {integer(1, 60), integer(1, 60)},
         begin
             S1 = calendar:datetime_to_gregorian_seconds(T),
             S2 = AddMinutes1 * 60,
             S3 = AddMinutes2 * 60,
             {calendar:gregorian_seconds_to_datetime(S1 + S2),
              calendar:gregorian_seconds_to_datetime(S1 + S2 + S3)}
         end).
    
    











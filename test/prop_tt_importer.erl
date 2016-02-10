-module(prop_tt_importer).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../src/talks_tweeter.hrl").


-define(OUT_FILE, "/tmp/test.csv").
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
-define(TITLES, ["'Erlang'",
                 "'Elixir'",
                 "Cooking",
                 "'Haskell'",
                 "'Scala'",
                 "teacher?",
                 "wife?",
                 "Keeping distance",
                 "Alice?"]).
-define(LOCATIONS, ["room 1", "room 2", "room 3", "Research Track"]).
-define(FIVE_PM_IN_MUNTES_FROM7, 10 * 60).

%%%===================================================================
%%% Properties
%%%===================================================================

prop_tt_importer_works_for_csv() ->
    proper:numtests(
      1000,
      ?FORALL(Talks,
              gen_talks(),
              ?TRAPEXIT(
                 begin
                     write_to_file(Talks, ?OUT_FILE),
                     tt_importer:start_link(),
                     tt_store:start_link(),
                     tt_importer:import_csv_file(?OUT_FILE, date()),
                     ?assertEqual(length(Talks),
                                  length(tt_store:list())),
                     collect(
                       length(Talks),
                       lists:all(
                         fun(#talk{title = T}) ->
                                 lists:keyfind(T, 1, Talks) =/= false
                         end, tt_store:list()))
                 end))).


%%%===================================================================
%%% Generators
%%%===================================================================

gen_talks() ->
    ?LET(Talks, non_empty(list(gen_talk())), remove_duplicates(Talks)).

gen_talk() ->
    {gen_title(), gen_location(), gen_day(), gen_minutes_from_7_am()}.

%% Helpers

gen_title() ->
    ?LET({Prefix, Suffix},
         {elements(?PREFIXES), elements(?TITLES)},
         begin
             Title = Prefix ++ Suffix,
             case string:chr(Title, $,) + string:chr(Title, $') of
                 0 ->
                     Title;
                 _ ->
                     [$"] ++ Title ++ [$"]
             end
         end).

double_quote(S) ->
    re:replace(S, "\".*\"", "\"&\"", [{return,list}]).

gen_location() ->
    elements(?LOCATIONS).

gen_day() ->
    integer(1,2).

gen_minutes_from_7_am() ->
    ?SUCHTHAT(Time,
              ?LET({I, X},
                   {gen_talk_interval(), integer(1, 20)},
                   60 + I*X),
              Time < ?FIVE_PM_IN_MUNTES_FROM7).

gen_talk_interval() ->
    frequency([{1, 60},
               {4, 25},
               {4, 45}]).

%%%===================================================================
%%% Generators Helpers
%%%===================================================================

remove_duplicates(Talks) ->
    remove_duplicates(Talks, []).

remove_duplicates([], Acc) ->
    Acc;
remove_duplicates([T | Rest], Acc) ->
    Title = element(1, T),
    case lists:keyfind(Title, 1, Rest) of
        false ->
            remove_duplicates(Rest, [T|Acc]);
        _Found ->
            remove_duplicates(Rest, Acc)
    end.


write_to_file(Talks, OutFilename) ->
    file:delete(OutFilename),
    lists:foreach(
      fun(Talk) ->
              Format = "~s,~s,~p,~p~n",
              T = io_lib:format(Format, tuple_to_list(Talk)),
              ok = file:write_file(OutFilename, T, [append])
      end, Talks).



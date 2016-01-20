-module(tt_importer_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([testImport/1]).

all() -> [testImport].


testImport(_) ->
    tt_store:start_link(),
    tt_importer:start_link(),
    tt_importer:import_file("../../../../talks.txt"),
    Res = tt_store:list(),
    {ok, Pwd} = file:get_cwd(),
    ct:print("Working directory: ~p", [Pwd]),
    Res = [{talk, "School of Erlang", {{2015,12,15}, {10,00,00}}, {{2015,12,15}, {11,00,00}}, "ESL Office - Room A"},
           {talk, "School of Elixir", {{2015,12,15}, {11,00,00}}, {{2015,12,15}, {12,00,00}}, "ESL Office - Room B"}].

-module(tt_importer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DATA_FILENAME, "talks.txt").

-compile([export_all]).

all() -> [testImport].

init_per_suite(Config) ->
    DataFile = filename:join([?config(data_dir, Config), ?DATA_FILENAME]),
    [{data_file, DataFile} | Config].

testImport(Config) ->
    %% GIVEN
    DataFile = ?config(data_file, Config),
    tt_store:start_link(),
    tt_importer:start_link(),

    %% WHEN
    tt_importer:import_file(DataFile),

    %% THEN
    Expected = lists:foldl(
                 fun(T, Acc) ->
                         [
                          list_to_tuple([talk | tuple_to_list(T)]
                                        ++ [false])
                          | Acc
                         ]
                 end, [], element(2, file:consult(DataFile))),
    ?assertEqual(lists:sort(Expected), lists:sort(tt_store:list())).
I

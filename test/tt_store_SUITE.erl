-module(tt_store_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

-export([all/0]).

all() -> [testReversedTimes,
          testList,
          testExactTime,
          testIncluded,
          testIncludedMultipleAll,
          testIncludedMultipleSome,
          testOverlappingAtStart,
          testOverlappingAtStartMultiple,
          testOverlappingAtEnd,
          testNotOverlapping,
          testNameFound,
          testNameNotFound].

testReversedTimes(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {12, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:list(),
    Res = [].

testList(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"),
    Res = tt_store:list(),
    Res = [{talk, "First talk",  {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"},
           {talk, "Second talk", {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"}].

testExactTime(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].

testIncluded(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 8, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].

testIncludedMultipleAll(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 8, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"},
           {talk, "Second talk",  {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"}].

testIncludedMultipleSome(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 8, 00, 0}}, {{2016, 5, 5}, {11, 30, 0}}),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].

testOverlappingAtStart(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 9, 00, 0}}, {{2016, 5, 5}, {10, 30, 0}}),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].

testOverlappingAtStartMultiple(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {10, 30, 0}}, {{2016, 5, 5}, {11, 30, 0}}, "Room 1"),
    tt_store:add("Third talk",    {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {11, 30, 0}}, "Room 1"),
    tt_store:add("Fourth talk",   {{2016, 5, 5}, {08, 00, 0}}, {{2016, 5, 5}, {10, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 9, 00, 0}}, {{2016, 5, 5}, {10, 30, 0}}),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"},
           {talk, "Second talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].

testOverlappingAtEnd(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, {10, 30, 0}}, {{2016, 5, 5}, {12, 00, 0}}),
    Res = [].

testNotOverlapping(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 3, 00, 0}}, {{2016, 5, 5}, { 8, 00, 0}}),
    Res = [].

testNameFound(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_name("First talk"),
    Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].

testNameNotFound(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_name("Unknown talk"),
    Res = [].

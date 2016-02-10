-module(tt_store_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(talk,  {title :: string(), start_time :: calendar:datetime(), end_time :: calendar:datetime(), location :: string(), published = false :: boolean()}).

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
          testNameNotFound,
          testModifySameName,
          testModifyDifferentName,
          testMarkPublished,
          testFindByTimeUnpublished].

testReversedTimes(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {12, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:list(),
    %%Res = [].
    ?assertEqual(Res, []).

testList(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"),
    Res = tt_store:list(),
    %%Res = [{talk, "First talk",  {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"},
    %%       {talk, "Second talk", {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"}].
    ?assertEqual(Res, [{talk, "First talk",  {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false},
                      {talk, "Second talk", {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2", false}]).

testExactTime(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].
    ?assertEqual(Res, [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false}]).

testIncluded(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 8, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].
    ?assertEqual(Res, [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false}]).

testIncludedMultipleAll(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 8, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"},
    %%       {talk, "Second talk",  {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"}].
    ?assertEqual(Res, [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false},
                        {talk, "Second talk",  {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2", false}]).

testIncludedMultipleSome(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 8, 00, 0}}, {{2016, 5, 5}, {11, 30, 0}}),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].
    ?assertEqual(Res, [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false},
                       {talk, "Second talk",  {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {12, 00, 0}}, "Room 2", false}]).

testOverlappingAtStart(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 9, 00, 0}}, {{2016, 5, 5}, {10, 30, 0}}),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].
    ?assertEqual(Res,  [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false}]).

testOverlappingAtStartMultiple(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    tt_store:add("Second talk",   {{2016, 5, 5}, {10, 30, 0}}, {{2016, 5, 5}, {11, 30, 0}}, "Room 1"),
    tt_store:add("Third talk",    {{2016, 5, 5}, {11, 00, 0}}, {{2016, 5, 5}, {11, 30, 0}}, "Room 1"),
    tt_store:add("Fourth talk",   {{2016, 5, 5}, {08, 00, 0}}, {{2016, 5, 5}, {10, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 9, 00, 0}}, {{2016, 5, 5}, {10, 30, 0}}),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"},
    %%       {talk, "Second talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].
    ?assertEqual(Res, [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false},
                      {talk, "Second talk",   {{2016, 5, 5}, {10, 30, 0}}, {{2016, 5, 5}, {11, 30, 0}}, "Room 1", false}]).

testOverlappingAtEnd(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, {10, 30, 0}}, {{2016, 5, 5}, {12, 00, 0}}),
    %%Res = [].
    ?assertEqual(Res, []).

testNotOverlapping(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_time(  {{2016, 5, 5}, { 3, 00, 0}}, {{2016, 5, 5}, { 8, 00, 0}}),
    %%Res = [].
    ?assertEqual(Res, []).

testNameFound(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_name("First talk"),
    %%Res = [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"}].
    ?assertEqual(Res, [{talk, "First talk",   {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1", false}]).

testNameNotFound(_) ->
    tt_store:start_link(),
    tt_store:add("First talk",    {{2016, 5, 5}, {10, 00, 0}}, {{2016, 5, 5}, {11, 00, 0}}, "Room 1"),
    Res = tt_store:find_by_name("Unknown talk"),
    %%Res = [].
    ?assertEqual(Res, []).

testModifySameName(_) ->
  %given
  OldTalk = #talk{
    title = "First talk",
    start_time = {{2016, 5, 5}, {10, 00, 0}},
    end_time = {{2016, 5, 5}, {11, 00, 0}},
    location = "Room 1"},
  NewTalk = OldTalk#talk{location = "Different Location"},

  %when
  tt_store:start_link(),
  tt_store:add(OldTalk#talk.title, OldTalk#talk.start_time, OldTalk#talk.end_time, OldTalk#talk.location),
  tt_store:modify(OldTalk#talk.title, NewTalk),
  [Res] = tt_store:list(),

  %then
  ?assertEqual(NewTalk, Res).

testModifyDifferentName(_) ->
  %given
  OldTalk = #talk{
    title = "First talk",
    start_time = {{2016, 5, 5}, {10, 00, 0}},
    end_time = {{2016, 5, 5}, {11, 00, 0}},
    location = "Room 1"},
  NewTalk = OldTalk#talk{title = "Different Title" , location = "Different Location"},

  %when
  tt_store:start_link(),
  tt_store:add(OldTalk#talk.title, OldTalk#talk.start_time, OldTalk#talk.end_time, OldTalk#talk.location),
  tt_store:modify(OldTalk#talk.title, NewTalk),
  [Res] = tt_store:list(),

  %then
  ?assertEqual(NewTalk, Res).

testMarkPublished(_) ->
  %given
  Talk = #talk{
    title = "First talk",
    start_time = {{2016, 5, 5}, {10, 00, 0}},
    end_time = {{2016, 5, 5}, {11, 00, 0}},
    location = "Room 1"},

  %when
  tt_store:start_link(),
  tt_store:add(Talk#talk.title, Talk#talk.start_time, Talk#talk.end_time, Talk#talk.location),
  tt_store:mark_published([Talk]),
  Res = tt_store:list(),

  %then
  ?assertEqual(Res, [Talk#talk{published = true}]).


testFindByTimeUnpublished(_) ->
  %given
  UnpublishedTalk = #talk{
    title = "Unpublished talk",
    start_time = {{2016, 5, 5}, {10, 00, 0}},
    end_time = {{2016, 5, 5}, {11, 00, 0}},
    location = "Room 1"},
  PublishedTalk = UnpublishedTalk#talk{title = "Published Talk"},

  %when
  tt_store:start_link(),
  tt_store:add(UnpublishedTalk#talk.title, UnpublishedTalk#talk.start_time, UnpublishedTalk#talk.end_time, UnpublishedTalk#talk.location),
  tt_store:add(PublishedTalk#talk.title, PublishedTalk#talk.start_time, PublishedTalk#talk.end_time, PublishedTalk#talk.location),
  tt_store:mark_published([PublishedTalk]),
  Res = tt_store:find_by_time_unpublished({{2016, 5, 5}, { 9, 00, 0}}, {{2016, 5, 5}, {12, 0, 0}}),

  %then
  ?assertEqual(Res, [UnpublishedTalk]).

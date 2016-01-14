-module(tt_store_suite).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test0/1, test1/1, test2/1, test3/1]).

all() -> [test0, test1, test2].

test0(_) ->
    tt_store:start_link(),
    tt_store:add("Bum", {10,0,0}, {11,0,0}, "Bah"),
    Res = tt_store:find_by_time({10,0,0}, {11,0,0}),
    Res = [{talk,"Bum",{10,0,0},{11,0,0},"Bah"}].


test1(_) ->
    tt_store:start_link(),
    tt_store:add("Bum", {10,0,0}, {11,0,0}, "Bah"),
    Res = tt_store:find_by_time({12,0,0}, {13,0,0}),
    Res = [].

test2(_) ->
    tt_store:start_link(),
    tt_store:add("Bum", {10,0,0}, {11,0,0}, "Bah"),
    Res = tt_store:find_by_name("Bum"),
    Res = [{talk,"Bum",{10,0,0},{11,0,0},"Bah"}].

test3(_) ->
    tt_store:start_link(),
    tt_store:add("Bum", {10,0,0}, {11,0,0}, "Bah"),
    Res = tt_store:find_by_name("E"),
    Res = [].

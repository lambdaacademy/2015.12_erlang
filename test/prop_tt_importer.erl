-module(prop_tt_importer).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-define(OUT_FILE, "/tmp/test.csv").

%%%===================================================================
%%% Properties
%%%===================================================================

prop_csv_is_imoprted() ->
    %% ?FORALL(Talks, list(gen_talks()), ?IMPLIES(no_duplicates(Talks), PropHere))
    %% or get rid of dupls in gen_talks()
    ?FORALL(N, integer(1, 50),
            ?TRAPEXIT(begin
                          talks_gen:gen_talks2(?OUT_FILE, N),
                          tt_importer:start_link(),
                          tt_store:start_link(),
                          tt_importer:import_csv_file(?OUT_FILE, date()),
                          ?assertEqual(count_line(?OUT_FILE),
                                       length(tt_store:list())),
                          collect(N, true)
                      end)).

count_line(Filename) ->
    case file:open(Filename, [read]) of
        {ok, IoDevice} ->
            Count = count_line(IoDevice, 0),
            file:close(IoDevice),
            Count;
        {error, Reason} ->
            io:format("~s open error  reason:~s~n", [Filename, Reason]),
            ng
    end.


count_line(IoDevice, Count) ->
    case file:read_line(IoDevice) of
        {ok, _} -> count_line(IoDevice, Count+1);
        eof     -> Count
    end.

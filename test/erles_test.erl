-module(erles_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erles/include/erles.hrl").

-define(STREAM_DOES_NOT_EXIST_VER, -1).

-define(assertReceived(Pattern, Timeout),
        ?assertMatch(Pattern,
                     receive X -> X after 1000 -> exit(timeout) end)).

connects_to_and_disconnects_from_evenstore_test() ->
    {ok, Pid} = erles:connect(node, {"localhost", 1113}),
    ?assert(is_pid(Pid)),
    ?assertMatch(ok, erles:ping(Pid)),
    ?assertMatch(ok, erles:close(Pid)).

appends_to_and_subsribes_to_stream_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(_ConnPid = C) ->
             [
              appends_to_stream_and_increments_version(C),
              appends_to_stream_and_gets_notifications_via_subscription(C),
              appends_with_latest_version_to_previously_deleted_stream(C)
             ]
     end}.


appends_to_stream_and_increments_version(ConnPid) ->
    fun() ->
            SId = stream_id(),
            Data =
                fun(Id) ->
                        event_data(<<"hello world ",
                                     (integer_to_binary(Id))/binary>>)
                end,
            [
             ?assertMatch(
                {ok, NewStreamVer},
                erles:append(ConnPid, SId, ExpStreamVer, [Data(NewStreamVer)]))
             || {ExpStreamVer, NewStreamVer} <- [{?STREAM_DOES_NOT_EXIST_VER, 0},
                                                 {0, 1},
                                                 {1, 2}]]
    end.

appends_to_stream_and_gets_notifications_via_subscription(ConnPid) ->
    fun() ->
            SId = stream_id(),
            Event = event_data(Data = <<"hello subscribe">>),
            {ok, _SubPid} = erles:subscribe(ConnPid, SId),
            {ok, StreamVer} = erles:append(ConnPid, SId, any, [Event]),
            ?assertReceived(
               {subscr_event,  _, #event{stream_id = SId, data = Data}, StreamVer},
               1000)
    end.

appends_with_latest_version_to_previously_deleted_stream(ConnPid) ->
    fun() ->
            SId = stream_id(),
            Event1 = event_data(<<"hello world 1">>),
            {ok, StreamVer} = erles:append(ConnPid, SId, any, [Event1]),
            ok = erles:delete(ConnPid, SId, StreamVer, soft),
            Event2 = event_data(<<"hello world 2">>),
            ExpStreamVer = StreamVer+1,
            ?assertMatch({ok, ExpStreamVer},
                         erles:append(ConnPid, SId, StreamVer, [Event2]))
    end.

%%% Fixtures

setup() ->
    {ok, Pid} = erles:connect(node, {"localhost", 1113}),
    Pid.

teardown(Pid) ->
    erles:close(Pid).

%%% Helpers

stream_id() ->
    BinUUID = erles_utils:binary_str(
                erles_utils:uuid_to_string(
                  erles_utils:gen_uuid())),
    <<"test-stream-", BinUUID/binary>>.

event_data(Data) ->
    #event_data{
       event_type = <<"test_event">>,
       data = Data
      }.

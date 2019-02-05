-module(es_user_srv_test).

-include_lib("eunit/include/eunit.hrl").

-define(COMPLETE_USER_PARAMS,
        [{name, <<"szymon">>}, {surname, "mentel"}, {age, 38}]).

-define(assertReceived(Pattern, Timeout),
        ?assertMatch(Pattern,
                     receive X -> X after Timeout -> exit(timeout) end)).

-define(assertNoMessages, receive Y -> exit({unexpected_message, Y})
                          after 0 -> ok end).

user_server_is_started_and_stopped_test() ->
    {ok, _} = es_user_events_manager:start(),
    {ok, Pid} = es_user_srv:start(),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, es_user_srv:stop(Pid)),
    ?assert(not is_process_alive(Pid)),
    es_user_events_manager:stop().

manipultes_users_test_() ->
    {foreach,
     fun setup/0,
     fun teardown/1,
     [
      fun(Pid) ->
              [
               creates_complete_user(Pid),
               creates_incomplete_user(Pid),
               updates_incomplete_user(Pid)
              ]
      end,
      fun(Pid) ->
              [
               lists_incomplete_users(Pid),
               incomplete_gets_completed(Pid)
              ]
      end]
    }.

losts_incomplete_user_on_restart_test() ->
    Pid = setup(),
    TestFun = losts_incomplete_user_on_restart(Pid),
    NewPid = TestFun(),
    teardown(NewPid).

creates_complete_user(Pid) ->
    fun() ->
            Me = self(),
            WhenUserCompleted = fun(UId) -> Me ! {user_completed, UId} end,
            ?assertMatch({ok, UserId} when is_binary(UserId),
                                           es_user_srv:new(Pid, ?COMPLETE_USER_PARAMS, WhenUserCompleted)),
            ?assertReceived({user_completed, UserId}, 1000)
    end.

creates_incomplete_user(Pid) ->
    fun() ->
            Me = self(),
            WhenUserCompleted = fun(UId) -> Me ! {user_completed, UId} end,
            ?assertMatch({ok, UserId} when is_binary(UserId),
                                           es_user_srv:new(Pid, tl(?COMPLETE_USER_PARAMS), WhenUserCompleted)),
            ?assertNoMessages
    end.

updates_incomplete_user(Pid) ->
    fun() ->
            Me = self(),
            WhenUserCompleted = fun(UId) -> Me ! {user_completed, UId} end,
            {ok, UserId} = es_user_srv:new(Pid, tl(?COMPLETE_USER_PARAMS), WhenUserCompleted),
            ?assertNoMessages,
            UpdateParam = hd(?COMPLETE_USER_PARAMS),
            UpdateParamName = element(1, UpdateParam),
            ?assertMatch({ok, [UpdateParamName]},
                         es_user_srv:update(Pid, UserId, [UpdateParam])),
            ?assertReceived({user_completed, UserId}, 1000)
    end.

lists_incomplete_users(Pid) ->
    fun() ->
            {ok, CompleteId} = es_user_srv:new(Pid, ?COMPLETE_USER_PARAMS, fun(_) -> ok end),
            {ok, IncompleteId1} = es_user_srv:new(Pid, tl(?COMPLETE_USER_PARAMS), fun(_) -> ok end),
            {ok, IncompleteId2} = es_user_srv:new(Pid, tl(?COMPLETE_USER_PARAMS), fun(_) -> ok end),
            {ok, Incompletes} = es_user_srv:get_incomplete(Pid),
            ?assertEqual(lists:sort([IncompleteId1, IncompleteId2]),
                         lists:sort(Incompletes))
    end.

incomplete_gets_completed(Pid) ->
    fun() ->
            {ok, UserId} = es_user_srv:new(Pid, tl(?COMPLETE_USER_PARAMS)),
            {ok, Incompletes1} = es_user_srv:get_incomplete(Pid),
            ?assert(lists:member(UserId, Incompletes1)),
            {ok, Completes1} = es_user_srv:get_complete(Pid),
            ?assertNot(lists:member(UserId, Completes1)),
            UpdateParam = hd(?COMPLETE_USER_PARAMS),
            {ok, _} = es_user_srv:update(Pid, UserId, [UpdateParam]),
            {ok, Incompletes2} = es_user_srv:get_incomplete(Pid),
            ?assertNot(lists:member(UserId, Incompletes2)),
            {ok, Completes2} = es_user_srv:get_complete(Pid),
            ?assert(lists:member(UserId, Completes2))
    end.

losts_incomplete_user_on_restart(Pid) ->
    fun() ->
            {ok, CompleteId} = es_user_srv:new(Pid, ?COMPLETE_USER_PARAMS),
            {ok, IncompleteId} = es_user_srv:new(Pid, tl(?COMPLETE_USER_PARAMS)),
            {ok, [IncompleteId]} = es_user_srv:get_incomplete(Pid),
            ok = es_user_srv:stop(Pid),
            {ok, NewPid} = es_user_srv:start(),
            {ok, []} = es_user_srv:get_incomplete(NewPid),
            NewPid
    end.

%% Fixtures


setup()->
    {ok, _} = es_user_events_manager:start(),
    {ok, Pid} = es_user_srv:start(),
    Pid.

teardown(Pid) ->
    es_user_srv:stop(Pid),
    es_user_events_manager:stop().

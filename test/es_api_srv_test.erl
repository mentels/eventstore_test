-module(es_api_srv_test).

-include("es.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(COMPLETE_USER_PARAMS,
        [{name, <<"szymon">>}, {surname, "mentel"}, {age, 38}]).

-define(assertReceived(Pattern, Timeout),
        ?assertMatch(Pattern,
                     receive X -> X after Timeout -> exit(timeout) end)).
-define(assertNoMessages, receive X -> exit({unexpected_message, X})
                          after 0 -> ok end).

es_api_srv_is_started_and_stopped_test() ->
    es_user_events_manager:start(),
    es_events_db:start(),
    {ok, Pid} = es_api_srv:start(),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, es_api_srv:stop()),
    ?assert(not is_process_alive(Pid)),
    es_events_db:stop(),
    es_user_events_manager:stop().

manipultes_users_test_() ->
    {timeout, 1, % 1s
     {foreach,
      fun setup/0,
      fun teardown/1,
      [
       [creates_complete_user(),
        creates_incomplete_user(),
        updates_incomplete_user()]
      ]}}.



creates_complete_user() ->
    fun() ->
            ok = es_user_events_manager:subscribe(test_events_handler, self()),
            ?assertMatch(
               {ok, UserId} when is_binary(UserId),
                                 es_api_srv:create_user(?COMPLETE_USER_PARAMS)),
            ?assertReceived({create_user, _}, 1000),
            ?assertReceived({user_created, _}, 1000),
            ?assertReceived({user_completed, _}, 1000),
            ?assertNoMessages,
            ok = es_user_events_manager:unsubscribe(test_events_handler, [])
    end.

creates_incomplete_user() ->
    fun() ->
            ok = es_user_events_manager:subscribe(test_events_handler, self()),
            ?assertMatch(
               {ok, UserId} when is_binary(UserId),
                                 es_api_srv:create_user(tl(?COMPLETE_USER_PARAMS))),
            ?assertReceived({create_user, _}, 1000),
            ?assertReceived({user_created, _}, 1000),
            ?assertNoMessages,
            ok = es_user_events_manager:unsubscribe(test_events_handler, [])
    end.


updates_incomplete_user() ->
    fun() ->
            ok = es_user_events_manager:subscribe(test_events_handler, self()),
            {ok, UserId} = es_api_srv:create_user(tl(?COMPLETE_USER_PARAMS)),
            UpdateParam = hd(?COMPLETE_USER_PARAMS),
            ?assertEqual({ok, UserId, [element(1, UpdateParam)]},
                         es_api_srv:update_user(UserId, [UpdateParam])),
            ?assertReceived({create_user, _}, 1000),
            ?assertReceived({user_created, _}, 1000),
            ?assertReceived({update_user, _, _}, 1000),
            ?assertReceived({user_updated, _, _}, 1000),
            ?assertReceived({user_completed, _}, 1000),
            ?assertNoMessages,
            ok = es_user_events_manager:unsubscribe(test_events_handler, [])
    end.

%% Fixtures

setup()->
    {ok, _} = es_events_db:start(),
    {ok, _} = es_user_events_manager:start(),
    {ok, Pid} = es_user_srv:start(),
    {ok, _} = es_api_srv:start(),
    Pid.

teardown(UserSrvPid) ->
    es_api_srv:stop(),
    es_user_srv:stop(UserSrvPid),
    es_user_events_manager:stop(),
    es_events_db:stop().

%% Helpers

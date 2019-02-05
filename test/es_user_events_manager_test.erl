-module(es_user_events_manager_test).

-include_lib("eunit/include/eunit.hrl").

-define(COMPLETE_USER_PARAMS,
        [{name, <<"szymon">>}, {surname, "mentel"}, {age, 38}]).

starts_and_stops_manager_test() ->
    {ok, Pid} = es_user_events_manager:start(),
    ?assert(is_pid(Pid)),
    ?assert(whereis(es_user_events_manager) == Pid),
    ?assertEqual(ok, es_user_events_manager:stop()),
    ?assert(not is_process_alive(Pid)).

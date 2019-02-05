-module(es_user_test).

-include("es.hrl").
-include_lib("eunit/include/eunit.hrl").

user_get_id_test() ->
    User = es_user:new([]),
    ?assert(is_binary(es_user:get_id(User))),
    ?assert(es_user:get_id(User) =:= User#es_user.id).

instanitates_incomplete_user_test() ->
    Incomplete = es_user:new([{name, "szymon"}]),
    ?assert(not es_user:is_complete(Incomplete)).

instanitates_complete_user_test() ->
    Complete = es_user:new([
                            {name, "szymon"},
                            {surname, "mentel"},
                            {age, 34}
                           ]),
    ?assert(es_user:is_complete(Complete)).

builds_incomplete_user_test() ->
    Incomplete = es_user:set_age(44,
                                 es_user:set_name(<<"szymon">>,
                                                  es_user:new([]))),
    ?assert(not es_user:is_complete(Incomplete)).

builds_complete_user_test() ->
    Complete =
        es_user:set_age(44,
                        es_user:set_surname(<<"mentel">>,
                                            es_user:set_name(<<"szymon">>,
                                                             es_user:new([])))),
    ?assert(es_user:is_complete(Complete)).

fails_to_build_user_without_id_test() ->
    BadUser = #es_user{name = "szymon", id = undefined},
    ?assertError({badarg, no_id}, es_user:set_name("jola", BadUser)),
    ?assertError({badarg, no_id}, es_user:set_surname("bolek", BadUser)),
    ?assertError({badarg, no_id}, es_user:set_age(36, BadUser)),
    ?assertError({badarg, no_id}, es_user:is_complete(BadUser)).

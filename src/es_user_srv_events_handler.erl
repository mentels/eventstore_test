-module(es_user_srv_events_handler).

-behaviour(gen_event).

-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2, code_change/3]).


init({UserServerPid, _}) ->
    {ok, UserServerPid};
init(UserServerPid) ->
    {ok, UserServerPid}.

terminate(_, _) ->
    ok.

handle_call(_, _) ->
    error(not_implemented).

handle_info(_, _) ->
    error(not_implemented).

code_change(_,_,_) ->
    error(not_implemented).

handle_event({event, Ref, {create_user, Params}}, UserServerPid) ->
    {ok, UserId} =
        es_user_srv:new(UserServerPid, Params, fun(_) -> ok end),
    emit_event(Ref, {user_created, UserId}),
    is_user_complete(UserServerPid, UserId)
        andalso emit_event(Ref, {user_completed, UserId}),
    {ok, UserServerPid};
handle_event({event, Ref, {update_user, UserId, Params}}, UserServerPid) ->
    {ok, UpdatedParams} =
        es_user_srv:update(UserServerPid, UserId, Params),
    emit_event(Ref, {user_updated, UserId, UpdatedParams}),
    is_user_complete(UserServerPid, UserId)
        andalso emit_event(Ref, {user_completed, UserId}),
    {ok, UserServerPid};
handle_event(_Unknown, UserServerPid) ->
    {ok, UserServerPid}.

emit_event(Ref, Payload) ->
    es_user_events_manager:emit({event, Ref, Payload}).

is_user_complete(UserServerPid, UserId) ->
    {ok, Incomplete} = es_user_srv:get_incomplete(UserServerPid),
    not lists:member(UserId, Incomplete).

-module(es_user_events_handler).

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
    {ok, UserId} = es_user_srv:new(UserServerPid, Params),
    emit_event(Ref, {user_created, UserId});
handle_event({user_updated, UserId, Params}, UserServerPid) ->
    ok;
handle_event({user_created, UserId, Params}, UserServerPid) ->
    ok.

emit_event(Ref, Payload) ->
    es_user_events_manager:emit({event, Ref, Payload}).

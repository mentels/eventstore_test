-module(es_api_srv_events_handler).

-behaviour(gen_event).

-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2, code_change/3]).

init(_) ->
    {ok, []}.

terminate(_, _) ->
    ok.

handle_call(_, _) ->
    error(not_implemented).

handle_info(_, _) ->
    error(not_implemented).

code_change(_,_,_) ->
    error(not_implemented).

handle_event({event, Ref, {user_created, UserId}} = Event, State) ->
    es_api_srv:handle_event(Event),
    {ok, State};
handle_event({event, Ref, {user_updated, UserId, UpdatedParams}} = Event, State) ->
    es_api_srv:handle_event(Event),
    {ok, State};
handle_event({event, Ref, {user_completed, UserId}} = Event, State) ->
    es_api_srv:handle_event(Event),
    {ok, State};
handle_event(_Unknown, State) ->
    {ok, State}.

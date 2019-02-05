-module(test_events_handler).

-behaviour(gen_event).

-export([init/1, terminate/2]).
-export([handle_event/2, handle_call/2, handle_info/2, code_change/3]).

init(ReportToPid) ->
    {ok, ReportToPid}.

terminate(_, _) ->
    ok.

handle_call(_, _) ->
    error(not_implemented).

handle_info(_, _) ->
    error(not_implemented).

code_change(_,_,_) ->
    error(not_implemented).


handle_event({event, Ref, Event}, ReportToPid) ->
    ReportToPid ! Event,
    {ok, ReportToPid}.

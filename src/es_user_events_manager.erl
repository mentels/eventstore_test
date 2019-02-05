-module(es_user_events_manager).

-define(NAME, ?MODULE).

-export([start/0, start_link/0, stop/0]).
-export([emit/1, subscribe/2, unsubscribe/2]).

start_link() ->
    gen_event:start_link({local, ?NAME}).

start() ->
    gen_event:start({local, ?NAME}).

stop() ->
    gen_event:stop(?NAME).

emit({event, _Ref, _Payload} = Event) ->
    gen_event:notify(?NAME, Event).

subscribe(Handler, Args) ->
    gen_event:add_handler(?NAME, Handler, Args).

unsubscribe(Handler, Args) ->
    gen_event:delete_handler(?NAME, Handler, Args).

-module(es_api_srv).

-behaviour(gen_server).

-define(NAME, ?MODULE).
-define(USERS_STREAM, users).
-define(USER_STREAM(UserId), <<"users-", UserId/binary>>).

-record(state, {
                pending = [] :: list()
               }).

-export([start/0, start_link/0, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([handle_info/2, code_change/3]).
-export([create_user/1, update_user/2, handle_event/1]).

%% API

start() ->
    gen_server:start({local, ?NAME}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?NAME}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?NAME).

create_user(Params) ->
    gen_server:call(?NAME, {create_user, Params}).

update_user(UserId, Params) ->
    gen_server:call(?NAME, {update_user, UserId, Params}).

handle_event({event, _, _} = Event) ->
    gen_server:cast(?NAME, Event).

%% Callbacks

init([]) ->
    subscribe_to_user_events(es_api_srv_events_handler),
    {ok, #state{}}.

terminate(_, _) ->
    unsubscribe_from_user_events(es_api_srv_events_handler),
    ok.

handle_call({create_user, _} = Command,  Ref = From, State) ->
    NewState = handle_command(Command, From, State),
    {noreply, State};
handle_call({update_user, _, _} = Command,  Ref = From, State) ->
    NewState = handle_command(Command, From, State),
    {noreply, State}.

handle_cast({event, ReplyTo = Ref, {user_created, UserId}} = Event, State) ->
    gen_server:reply(ReplyTo, {ok, UserId}),
    NewState = handle_event(Event, UserId, State),
    {noreply, NewState};
handle_cast({event, ReplyTo = Ref, {user_updated, UserId, UpdatedParams}} = Event,
            State) ->
    gen_server:reply(ReplyTo, {ok, UserId, UpdatedParams}),
    NewState = handle_event(Event, UserId, State),
    {noreply, NewState};
handle_cast({event, _Ref, {user_completed, UserId}} = Event, State) ->
    NewState = handle_event(Event, UserId, State),
    {noreply, NewState}.

handle_info(_, _) ->
    error(not_implemented).

code_change(_, _, _) ->
    error(not_implemented).

%% Helpers

handle_command(Cmd, Ref, #state{pending = Pending} = State) ->
    Event = {event, Ref, Cmd},
    store_event(?USERS_STREAM, Event, element(1, Cmd)),
    emit_event(Event),
    State#state{pending = [Event | Pending]}.

handle_event({event, Ref, Payload} = Event, UserId, #state{pending = Pending} = State) ->
    store_event(?USER_STREAM(UserId), Event, element(1, Payload)),
    State#state{pending = lists:keydelete(Ref, 1, Pending)}.

subscribe_to_user_events(UserEventsHandler) ->
    ok = es_user_events_manager:subscribe(UserEventsHandler, []).

unsubscribe_from_user_events(UserEventsHandler) ->
    ok = es_user_events_manager:unsubscribe(UserEventsHandler, []).

emit_event(Event) ->
    es_user_events_manager:emit(Event).

store_event(Stream, Event, Type) ->
    es_events_db:store(Stream, Event, Type).

-module(es_utils).

-export([uuid/0]).

uuid() ->
    erles_utils:binary_str(
      erles_utils:uuid_to_string(
        erles_utils:gen_uuid())).

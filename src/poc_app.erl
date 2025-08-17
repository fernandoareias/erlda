-module(poc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> 
    http_server_cache_stage:start_link(),
    http_server_response_writer_stage:start_link(),
    http_server_get_stage:start_link(),
    http_server_http_parser_stage:start_link(),
    http_server:start(),
    {ok, self()}.

stop(_State) ->
    ok.
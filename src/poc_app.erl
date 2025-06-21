-module(poc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) -> 
    get_stage:start_link(),
    http_parser_stage:start_link(),
    http_server:start(),
    {ok, self()}.

stop(_State) ->
    ok.
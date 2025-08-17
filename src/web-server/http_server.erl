-module(http_server).
-behaviour(ranch_protocol).
-export([start/0, start_link/3, start_link/4]).

-define(PORT, 8080).

start() ->
    io:format("[-][~p][~p] - Iniciando servidor na porta: ~p ~n", [calendar:local_time(), self(), ?PORT]),
    {ok, _} = ranch:start_listener(http_server,
        ranch_tcp,
        #{num_acceptors => 100,
          socket_opts => [
              {port, ?PORT},
              {backlog, 1024},
              {nodelay, true},
              {reuseaddr, true},
              {keepalive, true},
              {linger, {true, 0}},
              {recbuf, 262144},
              {sndbuf, 262144}
          ]},
        ?MODULE,
        []).

start_link(Ref, Socket, Transport, _Opts) ->
    Pid = spawn_link(fun() -> init(Ref, Socket, Transport) end),
    {ok, Pid}.

start_link(Ref, Transport, _Opts) ->
    Pid = spawn_link(fun() -> init_handshake(Ref, Transport) end),
    {ok, Pid}.

init(Ref, Socket, Transport) ->
    ok = ranch:accept_ack(Ref),
    case Transport:recv(Socket, 0, 15000) of
        {ok, Data} ->
            http_server_http_parser_stage ! {command, {parse_request, Data, Socket}, self()},
            wait_stage_and_exit();
        {error, _Reason} ->
            Transport:close(Socket),
            exit(normal)
    end.

init_handshake(Ref, Transport) ->
    {ok, Socket} = ranch:handshake(Ref),
    case Transport:recv(Socket, 0, 15000) of
        {ok, Data} ->
            http_server_http_parser_stage ! {command, {parse_request, Data, Socket}, self()},
            wait_stage_and_exit();
        {error, _Reason} ->
            Transport:close(Socket),
            exit(normal)
    end.

wait_stage_and_exit() ->
    receive
        {stage_result, _} -> ok;
        {stage_error, _} -> ok
    after 5000 -> ok
    end,
    exit(normal).

 
 
-module(http_server).
-export([start/0]).

-define(PORT, 8080).

start() ->
    io:format("[+][~p][~p] - Web server request processor listening... ~n", [calendar:local_time(), self()]),

    case gen_tcp:listen(?PORT, [{active, true}, binary]) of 
        {ok, ListenSocket} -> 
            io:format("[+][~p][~p] - Web server accepts messages now! ~n", [calendar:local_time(), self()]),
            loop(ListenSocket);
        {error, eaddrinuse} -> 
            io:format("[-][~p][~p] - Error port already in use ~n", [calendar:local_time(), self()]),
            error;
        _ ->            
            io:format("[-][~p][~p] - Error when start web server ~n", [calendar:local_time(), self()]),
            error
    end.

loop(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    io:format("Connection accepted at ~p~n", [calendar:local_time()]),

    receive
        {tcp, AcceptSocket, Data} ->
            http_parser_stage ! {command, {parse_request, Data, AcceptSocket}, {tcp, AcceptSocket}}, 
            loop(ListenSocket);
        {tcp_closed, AcceptSocket} ->
            io:format("[-][~p][~p] - Conexão fechada: ~p ~n", [calendar:local_time(), self(), AcceptSocket]),
            loop(ListenSocket);
        {tcp_error, AcceptSocket, Reason} ->
            io:format("[-][~p][~p] - Conexão erro: ~p ~n", [calendar:local_time(), self(), Reason]),
            loop(ListenSocket)
    end.

 
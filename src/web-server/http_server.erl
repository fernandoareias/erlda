-module(http_server).
-export([start/0]).

-define(PORT, 8080).

start() ->
    case gen_tcp:listen(?PORT, [{active, true}, binary]) of 
        {ok, ListenSocket} -> 
            io:format("[+][~p][~p] - Iniciando servidor na porta ~p ~n", [calendar:local_time(), self(), ?PORT]),
            loop(ListenSocket);
        {error, eaddrinuse} -> 
            io:format("[-][~p][~p] - Erro a porta já está sendo utilizada ~n", [calendar:local_time(), self()]),
            error;
        _ ->            
            error
    end.

loop(ListenSocket) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),

    receive
        {tcp, AcceptSocket, Data} ->
            http_parser_stage ! {command, {parse_request, Data, AcceptSocket}, {tcp, AcceptSocket}}, 
            loop(ListenSocket);
        {tcp_closed, AcceptSocket} ->
            loop(ListenSocket);
        {tcp_error, AcceptSocket, Reason} ->
            io:format("[-][~p][~p] - Conexão erro: ~p ~n", [calendar:local_time(), self(), Reason]),
            loop(ListenSocket)
    end.

 
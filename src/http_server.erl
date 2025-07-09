-module(http_server).
-export([start/0, start_server/0]).

-define(PORT, 8080).

 
-record(state, {
    socket,
    acceptor_sup :: pid() | undefined,
    ref :: reference() | undefined
}).

start() ->
    Pid = spawn(?MODULE, start_server, []),
    register(?MODULE, Pid).

start_server() ->
    Options = [
        binary,
        {reuseaddr, true},
        {keepalive, true},
        {backlog, 128},
        {active, false}
    ],
    handle_listening(gen_tcp:listen(?PORT, Options)).


handle_listening({ok, Socket}) ->
    case inet:sockname(Socket) of 
        {ok, {_IP, Port}} ->
            io:format("[http_server] Socket escutando na porta ~p~n", [Port]),
            {ok, Ref} = prim_inet:async_accept(Socket, -1),
            State =  #state{socket = Socket, ref = Ref},
            loop(State);
        {error, Reason} ->
            io:format("Socket não está escutando: ~p~n", [Reason]),
            gen_tcp:close(Socket),
            {stop, Reason}
    end;
handle_listening({error, Reason}) ->
    io:format("Falha ao iniciar listener: ~p~n", [Reason]),
    {stop, Reason}.
    
loop(State) ->
    receive
        {inet_async, _ListenSocket, _Ref, {ok, ClientSocket}} ->
            io:format("***** RECEBEU LISTEN SOCKER ~p E CLIENTE SOCKET ~p e Ref ~p ~n", [_ListenSocket, _Ref, ClientSocket]),
            gen_tcp:send(_ListenSocket, <<"Ola">>),
            io:format("[http_server] Novo acceptor para cliente ~p~n", [ClientSocket]),
            case http_server_acceptor:start_acceptor(ClientSocket, State#state.socket) of
                {ok, AcceptorPid} ->
                    ok = gen_tcp:controlling_process(ClientSocket, AcceptorPid),
                    {ok, NewRef} = prim_inet:async_accept(_ListenSocket, -1),
                    loop(State#state{ref = NewRef})
            end;
        {inet_async, _ListenSocket, _Ref, Error} ->
            io:format("Erro no acceptor do socket: ~p~n", [Error]),
            {stop, exceeded_accept_retry_count, State};
        _Other ->
            io:format("[http_server] Mensagem inesperada no loop ~p~n", [_Other])
    end.

 
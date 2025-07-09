-module(http_server_acceptor).

-record(state, {
    clientSocket,
    listenSocket
}).

-export([start_acceptor/2, init/1]).

start_acceptor(ClientSocket, ListenSocket) ->
    io:format("[acceptor] Iniciando acceptor para cliente ~p~n", [ClientSocket]),
    State = #state{listenSocket = ListenSocket, clientSocket = ClientSocket},
    Pid = spawn(?MODULE, init, [State]),
    {ok, Pid}.

init(State) ->
    case inet:peername(State#state.clientSocket) of
        {ok, {_, _}} ->
            case inet:setopts(State#state.clientSocket, [{active, true}]) of
                ok ->
                    io:format("Socket configurado para modo ativo~n"),
                    loop(State);
                {error, Reason} ->
                    io:format("Falha ao configurar socket para modo ativo: ~p~n", [Reason]),
                    gen_tcp:close(State#state.clientSocket),
                    {stop, Reason}
            end;
        {error, Reason} ->
            io:format("Acceptor ~p: Falha ao obter endereço do cliente: ~p~n", [self(), Reason]),
            gen_tcp:close(State#state.clientSocket),
            {stop, Reason}
    end.

loop(State) ->
    receive
        {tcp, Socket, Data} ->
            io:format("[acceptor] Recebido dados do socket ~p: ~p~n", [Socket, Data]),
            http_parser_stage ! {command, {parse_request, Data, Socket, self()}, {tcp, self()}},
            loop(State);
        {tcp_response, Socket, Response} ->
            io:format("[acceptor] Recebido pedido para enviar resposta para o socket ~p~n", [Socket]),
            io:format("[acceptor] Tamanho da resposta: ~p bytes~n", [byte_size(Response)]),
            case erlang:port_info(Socket) of
                undefined ->
                    io:format("[acceptor] Socket ~p está fechado ou inválido!~n", [Socket]);
                Info ->
                    io:format("[acceptor] port_info: ~p~n", [Info]),
                    case gen_tcp:send(Socket, Response) of
                        ok ->
                            io:format("[acceptor] Resposta enviada com sucesso para o socket ~p~n", [Socket]);
                        {error, Reason} ->
                            io:format("[acceptor] Falha ao enviar resposta: ~p~n", [Reason]);
                        {error, closed} ->
                            io:format("[acceptor] Fechado ~n");
                        {'EXIT', ExitReason} ->
                            io:format("[acceptor] Exceção ao enviar resposta: ~p~n", [ExitReason])
                    end
            end,
            loop(State);  
        {tcp_closed, Socket} ->
            io:format("[acceptor] Conexão fechada para socket ~p~n", [Socket]),
            gen_tcp:close(Socket),
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("[acceptor] Erro TCP no socket ~p: ~p~n", [Socket, Reason]),
            gen_tcp:close(Socket),
            io:format("[acceptor] Erro na conexão: ~p", [Reason])
    end.
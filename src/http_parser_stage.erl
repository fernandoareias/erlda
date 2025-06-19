-module(http_parser_stage).

-behaviour(stage_behaviour).

-export([start_link/0]).
-export([handle_command/1]).

-define(CRLF, "\r\n").
-define(AUTH_REALM, "RESTRITO").
-define(AUTH_CREDENTIALS, "admin:admin"). 

start_link() ->
    stage_behaviour:spawn_stage(?MODULE).

handle_command({parse_request, Data, Connection}) ->
    io:format("Recebeu request ~p | data: ~p ~n", [Connection, Data]),
    case process_request(Data, Connection) of 
        {get_stage, Path, Connection} ->
            case whereis(get_stage) of
                undefined -> 
                    gen_tcp:send(Connection, "HTTP/1.1 404 Not Found\r\n\r\n"),
                    gen_tcp:close(Connection),
                    {error, no_get_stage};
                Pid ->
                    {forward, Pid, {Path, Connection}}
            end;
        {error, Reason} ->
            {error, Reason}  
    end.


%%%===================================================================
%% Funções privadas
%%%===================================================================

process_request(Data, Connection) when is_binary(Data), is_port(Connection) ->
    io:format("[+][~p][~p] - Processing request...~n", [calendar:local_time(), self()]),
    try
        {Method, Path, _} = parse_request(Data),
        io:format("[+][~p][~p] - Method: ~p | Path: ~p ~n", [calendar:local_time(), self(), Method, Path]),
        Authenticated = true,
        route_request(Method, Path, Authenticated, Data, Connection) 
    catch
        error:Reason ->
            io:format("[!][~p][~p] - Error processing request: ~p~n", [calendar:local_time(), self(), Reason]),            
            gen_tcp:close(Connection),
            {error, Reason}
    end;
process_request(_InvalidData, _InvalidConnection) ->
    io:format("[!][~p][~p] - Invalid data or connection in process_request~n", [calendar:local_time(), self()]).

route_request(<<"GET">>, Path, _, _, Connection) ->
    io:format("[+][~p][~p] - Send data to GET Process...~n", [calendar:local_time(), self()]),
    {get_stage, Path, Connection}.


parse_request(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<?CRLF>>, [global]),
    case Lines of
        [RequestLine | HeaderLines] ->
            case parse_request_line(RequestLine) of
                {error, _} ->
                    io:format("[-][~p][~p] - Error parsing request line~n", [calendar:local_time(), self()]),
                    {error, "/", []};
                {Method, Path} ->
                    Headers = parse_headers(HeaderLines),
                    {Method, Path, Headers}
            end;
        _ ->
            io:format("[-][~p][~p] - Invalid request format~n", [calendar:local_time(), self()]),
            {error, "/", []}
    end;
parse_request(_InvalidData) ->
    io:format("[-][~p][~p] - Invalid request data~n", [calendar:local_time(), self()]),
    {error, "/", []}.

parse_request_line(RequestLine) ->
    case string:split(RequestLine, " ", all) of
        [Method, Path | _] -> {Method, Path};
        _ -> 
            io:format("[-][~p][~p] - Error parsing request line~n", [calendar:local_time(), self()]),
            {error, "/"}
    end.

parse_headers(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case binary:split(Line, <<": ">>) of
            [Key, Value] ->
                [{binary_to_list(Key), binary_to_list(Value)} | Acc];
            _ ->
                io:format("[+][~p][~p] - Ignoring invalid header line: ~p~n", [calendar:local_time(), self(), Line]),
                Acc
        end
    end, [], Lines).
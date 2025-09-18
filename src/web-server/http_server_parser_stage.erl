-module(http_server_parser_stage).

-behaviour(stage_behaviour).

-export([start_link/0]).
-export([handle_command/1]).

-define(CRLF, "\r\n").

start_link() ->
    stage_behaviour:spawn_stage(?MODULE).

handle_command({parse_request, Data, Socket}) ->
    case process_request(Data, Socket) of 
        {cache_stage, Path, Sock} ->
            {forward, http_server_cache_stage, {get, Path, Sock}};
        {error, Reason} ->
            {error, Reason}  
    end.

process_request(Data, Socket) when is_binary(Data) ->
    try
        {Method, Path, _} = parse_request(Data),
        Authenticated = true,
        route_request(Method, Path, Authenticated, Data, Socket) 
    catch
        error:Reason ->
            close_if_open(Socket),
            {error, Reason}
    end;
process_request(_InvalidData, _InvalidConnection) ->
    io:format("[!][~p][~p] - Invalid data or connection in process_request~n", [calendar:local_time(), self()]).

route_request(<<"GET">>, Path, _, _, Socket) ->
    {cache_stage, Path, Socket}.

parse_request(Data) when is_binary(Data) ->
    Lines = binary:split(Data, <<?CRLF>>, [global]),
    case Lines of
        [RequestLine | HeaderLines] ->
            case parse_request_line(RequestLine) of
                {error, _} ->
                    {error, "/", []};
                {Method, Path} ->
                    Headers = parse_headers(HeaderLines),
                    {Method, Path, Headers}
            end;
        _ ->
            {error, "/", []}
    end;
parse_request(_InvalidData) ->
    {error, "/", []}.

parse_request_line(RequestLine) ->
    case binary:split(RequestLine, <<" ">>, [global, trim]) of
        [Method, Path, _Version] -> {Method, Path};
        [Method, Path] -> {Method, Path};
        _ -> 
            {error, "/"}
    end.

parse_headers(Lines) ->
    lists:foldl(fun(Line, Acc) ->
        case binary:split(Line, <<": ">>) of
            [Key, Value] ->
                [{binary_to_list(Key), binary_to_list(Value)} | Acc];
            _ ->
                Acc
        end
    end, [], Lines).

close_if_open(Socket) when is_port(Socket) ->
    catch gen_tcp:close(Socket);
close_if_open(_) ->
    ok.



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
    case process_request(Data, Connection) of 
        {get_stage, Path, Connection} ->
            {forward, get_stage, {Path, Connection}};
        {error, Reason} ->
            {error, Reason}  
    end.


%%%===================================================================
%% Funções privadas
%%%===================================================================

process_request(Data, Connection) when is_binary(Data), is_port(Connection) ->
    try
        {Method, Path, _} = parse_request(Data),
        Authenticated = true,
        route_request(Method, Path, Authenticated, Data, Connection) 
    catch
        error:Reason ->
            gen_tcp:close(Connection),
            {error, Reason}
    end;
process_request(_InvalidData, _InvalidConnection) ->
    io:format("[!][~p][~p] - Invalid data or connection in process_request~n", [calendar:local_time(), self()]).

route_request(<<"GET">>, Path, _, _, Connection) ->
    {get_stage, Path, Connection}.


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
    case string:split(RequestLine, " ", all) of
        [Method, Path | _] -> {Method, Path};
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
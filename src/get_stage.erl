-module(get_stage).

-behaviour(stage_behaviour).

-export([start_link/0]).
-export([handle_command/1]).
-define(CRLF, "\r\n").
-define(AUTH_REALM, "RESTRITO").
-define(AUTH_CREDENTIALS, "admin:admin").

start_link() ->
    stage_behaviour:spawn_stage(?MODULE).

handle_command({Path, Connection}) ->
    io:format("[+][~p][~p] - Tentando obter o arquivo ~p | Connection ~p ~n", [calendar:local_time(), self(), Path, Connection]),
    case read_file(Path) of 
        {ok, Content} ->
            io:format("[+][~p][~p] - Encontrou o arquivo ~n", [calendar:local_time(), self()]),
            write_response(Connection, "200 OK", "text/html", Content),
            {ok, sent};
        {error, _} -> 
            write_response(Connection, "404 Not Found", "text/html", not_found_response()),
            io:format("[-][~p][~p] - Não encontrou o arquivo ~n", [calendar:local_time(), self()]),
            {error, "Não encontrou o arquivo"}
    end.



read_file(Path) -> 
    case file:read_file("./www" ++ binary_to_list(Path)) of
        {ok, Content} ->
            {ok, Content};
        {error, Reason} ->
            {error, Reason}
    end.

not_found_response() ->
    <<"<html><head><title>Not Found</title></head><body>Not Found</body></html>">>.

write_response(Connection, Status, ContentType, Body) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    Date = io_lib:format("~s, ~2..0w ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT",
                            [day_of_week(Year, Month, Day), Day, month(Month), Year, Hour, Minute, Second]),
    BinaryBody = iolist_to_binary(Body),
    ContentLength = integer_to_list(byte_size(BinaryBody)),
    Headers = [
        "HTTP/1.1 ", Status, ?CRLF,
        "Date: ", Date, ?CRLF,
        "Server: MyErlangServer", ?CRLF,
        "Content-Type: ", ContentType, ?CRLF,
        "Content-Length: ", ContentLength, ?CRLF,
        "Connection: close", ?CRLF,
        ?CRLF
    ],
    io:format("[+][~p][~p] - Connection ~p | Status response ~p ~n", [calendar:local_time(), self(), Connection, Status]),
    Response = list_to_binary([Headers, BinaryBody]),
    case gen_tcp:send(Connection, Response) of
        ok ->
            io:format("[+][~p][~p] - Response sent successfully~n", [calendar:local_time(), self()]);
        {error, Reason} ->
            io:format("[-][~p][~p] - Failed to send response: ~p~n", [calendar:local_time(), self(), Reason])
    end,
    gen_tcp:close(Connection),
    io:format("[+][~p][~p] - Connection closed~n", [calendar:local_time(), self()]).


day_of_week(Y, M, D) ->
    Days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    DayNum = calendar:day_of_the_week(Y, M, D),
    lists:nth(DayNum, Days).

month(M) ->
    Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    lists:nth(M, Months).
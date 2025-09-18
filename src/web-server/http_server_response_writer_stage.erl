-module(http_server_response_writer_stage).

-behaviour(stage_behaviour).

-export([start_link/0]).
-export([handle_command/1]).

-define(CRLF, "\r\n").

start_link() ->
    ensure_date_table(),
    spawn(fun update_date_loop/0),
    stage_behaviour:spawn_stage(?MODULE).

handle_command({Socket, Status, ContentType, Body}) ->
    try
        BinaryBody = iolist_to_binary(Body),
        ContentLength = integer_to_list(byte_size(BinaryBody)),
        Date = get_cached_date(),
        Headers = [
            "HTTP/1.1 ", Status, ?CRLF,
            "Date: ", Date, ?CRLF,
            "Server: MyErlangServer", ?CRLF,
            "Content-Type: ", ContentType, ?CRLF,
            "Content-Length: ", ContentLength, ?CRLF,
            "Connection: keep-alive", ?CRLF,
            ?CRLF
        ],
        Response = list_to_binary([Headers, BinaryBody]),
        _ = gen_tcp:send(Socket, Response),
        {ok, sent}
    catch
        _:Reason ->
            {error, Reason}
    end.

day_of_week(Y, M, D) ->
    Days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"],
    DayNum = calendar:day_of_the_week(Y, M, D),
    lists:nth(DayNum, Days).

month(M) ->
    Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    lists:nth(M, Months).

ensure_date_table() ->
    case ets:info(http_server_date_cache) of
        undefined -> ets:new(http_server_date_cache, [set, public, named_table]);
        _ -> ok
    end.

update_date_loop() ->
    ets:insert(http_server_date_cache, {date, compute_date()}),
    timer:sleep(1000),
    update_date_loop().

get_cached_date() ->
    case ets:lookup(http_server_date_cache, date) of
        [{date, D}] -> D;
        [] -> compute_date()
    end.

compute_date() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    io_lib:format("~s, ~2..0w ~s ~4..0w ~2..0w:~2..0w:~2..0w GMT",
                  [day_of_week(Year, Month, Day), Day, month(Month), Year, Hour, Minute, Second]).



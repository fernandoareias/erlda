-module(http_server_get_stage).

-behaviour(stage_behaviour).

-export([start_link/0]).
-export([handle_command/1]).

start_link() ->
    stage_behaviour:spawn_stage(?MODULE).

handle_command({Path, Socket}) ->
    case read_file(Path) of 
        {ok, Content} ->
            {forward, http_server_cache_stage, {put_and_respond, Path, Socket, "200 OK", "text/html", Content}};
        {error, _} -> 
            {forward, http_server_cache_stage, {put_and_respond, Path, Socket, "404 Not Found", "text/html", not_found_response()}}
    end.

read_file(Path) -> 
    Sanitized = sanitize_path(Path),
    FullPath = filename:join(["www", Sanitized]),
    io:format("[-][~p][~p] - Lendo arquivo ~p ~n", [calendar:local_time(), self(), FullPath]),  
    case file:read_file(FullPath) of
        {ok, Content} ->
            {ok, Content};
        {error, Reason} ->
            {error, Reason}
    end.

sanitize_path(PathBin) when is_binary(PathBin) ->
    [PathOnly | _] = binary:split(PathBin, <<"?">>, [global]),
    Sanitized0 = case PathOnly of
        <<"/">> -> <<"index.html">>;
        <<>> -> <<"index.html">>;
        _ ->
            case PathOnly of
                <<"/", Rest/binary>> -> Rest;
                _ -> PathOnly
            end
    end,
    case binary:matches(Sanitized0, <<"..">>) of
        [] -> binary_to_list(Sanitized0);
        _ -> "index.html"
    end.

not_found_response() ->
    <<"<html><head><title>Not Found</title></head><body>Not Found</body></html>">>.



-module(http_server_cache_stage).

-behaviour(stage_behaviour).

-export([start_link/0]).
-export([handle_command/1]).

start_link() ->
    stage_behaviour:spawn_stage(?MODULE).

handle_command({get, Path, Socket}) ->
    ensure_cache_table(),
    case lookup_cache(Path) of
        {ok, Content} ->
            {forward, http_server_response_writer_stage, {Socket, "200 OK", "text/html", Content}};
        not_found ->
            {forward, http_server_get_stage, {Path, Socket}}
    end;
handle_command({put_and_respond, Path, Socket, Status, ContentType, Body}) ->
    ensure_cache_table(),
    case Status of
        "200 OK" -> insert_cache(Path, Body);
        _ -> ok
    end,
    {forward, http_server_response_writer_stage, {Socket, Status, ContentType, Body}}.

ensure_cache_table() ->
    case ets:info(erlda_cache_table) of
        undefined ->
            ets:new(erlda_cache_table, [set, public, named_table, {write_concurrency, true}, {read_concurrency, true}]);
        _ -> ok
    end.

lookup_cache(PathBin) ->
    Sanitized = sanitize_path(PathBin),
    case ets:lookup(erlda_cache_table, Sanitized) of
        [{Sanitized, Content}] -> {ok, Content};
        [] -> not_found
    end.

insert_cache(PathBin, Content) ->
    Sanitized = sanitize_path(PathBin),
    ets:insert(erlda_cache_table, {Sanitized, Content}),
    ok.

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
    end;
sanitize_path(Path) when is_list(Path) ->
    Path.



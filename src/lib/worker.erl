-module(worker).

-export([spawn_worker/1, loop/1]).

spawn_worker(StageModule) when is_atom(StageModule) ->
    spawn(?MODULE, loop, [StageModule]).

loop(StageModule) ->
    receive
        {work, Command, From} ->
            case StageModule:handle_command(Command) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    io:format("[-][~p][~p] - Erro motivo ~p ~n", [calendar:local_time(), self(), Reason]);
                {forward, NextStagePid, NewCommand} ->
                    NextStagePid ! {command, NewCommand, From}
            end,
            loop(StageModule)
    end.

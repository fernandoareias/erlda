-module(stage_behaviour).

-export([
    spawn_stage/1,
    loop/2,
    add_worker/1,
    remove_worker/2,
    wait_message/1,
    count_workers/1
]).

-define(is_false(X), ((X) == false)).

-callback handle_command(Command :: term()) ->
    {ok, Result :: term()} | {error, Reason :: term()} |
    {forward, NextStagePid :: pid(), NewCommand :: term()}.

-callback rebalancing_policy() ->
    ok | {error, term()}.

-optional_callbacks([rebalancing_policy/0]).

spawn_stage(StageModule) when is_atom(StageModule) ->
    EtsTable = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    ets:new(EtsTable, [named_table, public, {write_concurrency, true}]),    
    Pid = spawn_link(?MODULE, loop, [StageModule, false]),
    register(StageModule, Pid),
    % ControllerPid = stage_controller:spawn_controller(StageModule, Pid),
    % link(ControllerPid), 
    Pid.

loop(StageModule, HasWorkers) when ?is_false(HasWorkers) ->
    WorkerPids = start_workers(StageModule, 10),
    EtsTable = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    ets:insert(EtsTable, {workers, WorkerPids}),
    loop(StageModule, true);
loop(StageModule, _)  ->
    receive  
        {command, Command, From} ->
            io:format("Recebeu o comand, enviando para ~p o comando ~p from ~p~n", [StageModule, Command, From]),
            [{workers, WorkersList}] = ets:lookup(list_to_atom(atom_to_list(StageModule) ++ "_workers"), workers),
            WorkerPid = pick_random_worker(WorkersList),
            WorkerPid ! {work, Command, From},
            loop(StageModule, true);
        {worker_ready, _WorkerPid} ->
            loop(StageModule, true);
        {'DOWN', _Ref, process, WorkerPid, _} ->
            remove_worker(StageModule, WorkerPid),
            loop(StageModule, true);
        stop ->
            erlang:hibernate(StageModule, wait_message, [StageModule])
    after 0 ->
        {message_queue_len, QLen} = process_info(self(), message_queue_len),
        if QLen > 0 ->
            io:format("DEBUG: QLen=~p mas não há mensagens no receive~n", [QLen]);
        true -> ok
        end,
        loop(StageModule, true)
    end.

wait_message(StageModule) ->
    receive
        stop ->
            io:format("[-][~p][~p] - Estágio parado.~n", [calendar:local_time(), self()]);
        Msg ->
            io:format("Recebido: ~p~n", [Msg]),
            loop(StageModule, true)
    end.

start_workers(StageModule, Count) when Count > 0 ->
    [spawn_worker(StageModule) || _ <- lists:seq(1, Count)].

spawn_worker(StageModule) ->
    worker:spawn_worker(StageModule).
 

pick_random_worker(Workers) ->
    N = length(Workers),
    Index = rand:uniform(N),
    lists:nth(Index, Workers).


add_worker(StageModule) ->
    EtsTable = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    [{workers, CurrentWorkers}] = ets:lookup(EtsTable, workers),
    WorkerPid = spawn_worker(StageModule),
    NewWorkers = [WorkerPid | CurrentWorkers],
    ets:insert(EtsTable, {workers, NewWorkers}).

remove_worker(StageModule, WorkerPid) when is_pid(WorkerPid) ->
    EtsTable = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    [{workers, CurrentWorkers}] = ets:lookup(EtsTable, workers),
    NewWorkers = lists:filter(fun(Pid) -> Pid /= WorkerPid end, CurrentWorkers),
    ets:insert(EtsTable, {workers, NewWorkers}),
    exit(WorkerPid, shutdown);
remove_worker(StageModule, _) ->
    EtsTable = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    [{workers, CurrentWorkers}] = ets:lookup(EtsTable, workers),
    MinWorkers = 1, 
    case length(CurrentWorkers) > MinWorkers of 
        true ->
            [RemovedWorker | NewWorkers] = lists:reverse(CurrentWorkers),
            exit(RemovedWorker, shutdown), 
            ets:insert(EtsTable, {workers, NewWorkers});
        false ->
            io:format("Não é possível remover mais workers. Mínimo atingido.~n")
    end.

count_workers(StageModule) ->
    try
        EtsTable = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
        [{workers, WorkersList}] = ets:lookup(EtsTable, workers),
        lists:foldl(fun(WorkerPid, Count) ->
            case is_process_alive(WorkerPid) of
                true -> Count + 1;
                false -> Count
            end
        end, 0, WorkersList)
    catch
        _:_ -> 0
    end.
    
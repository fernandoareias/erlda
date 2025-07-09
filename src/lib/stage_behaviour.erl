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
    ensure_workers_table(),
    Pid = spawn_link(?MODULE, loop, [StageModule, false]),
    register(StageModule, Pid),
    ControllerPid = stage_controller:spawn_controller(StageModule, Pid),
    link(ControllerPid), 
    Pid.


loop(StageModule, HasWorkers) when ?is_false(HasWorkers) ->
    WorkerPids = start_workers(StageModule, 10),
    StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    ets:insert(erlda_workers_table, {StageKey, WorkerPids}),
    loop(StageModule, true);
loop(StageModule, _)  ->
    receive  
        {command, Command, From} ->
            StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
            [{StageKey, WorkersList}] = ets:lookup(erlda_workers_table, StageKey),
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
    end.


ensure_workers_table() ->
    case ets:info(erlda_workers_table) of
        undefined ->
            ets:new(erlda_workers_table, [set, public, named_table, {write_concurrency, true}]);
        _ -> ok
    end.
    
wait_message(StageModule) ->
    receive
        stop ->
            io:format("[-][~p][~p] - Estágio parado.~n", [calendar:local_time(), self()]);
        _ ->
            io:format("[+][~p][~p] - Estágio saindo da hibernação ~n", [calendar:local_time(), self()]),
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
    StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    [{StageKey, CurrentWorkers}] = ets:lookup(erlda_workers_table, StageKey),
    WorkerPid = spawn_worker(StageModule),
    NewWorkers = [WorkerPid | CurrentWorkers],
    ets:insert(erlda_workers_table, {StageKey, NewWorkers}).

remove_worker(StageModule, WorkerPid) when is_pid(WorkerPid) ->
    StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    [{StageKey, CurrentWorkers}] = ets:lookup(erlda_workers_table, StageKey),
    NewWorkers = lists:filter(fun(Pid) -> Pid /= WorkerPid end, CurrentWorkers),
    ets:insert(erlda_workers_table, {StageKey, NewWorkers}),
    exit(WorkerPid, shutdown);
remove_worker(StageModule, _) ->
    StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    [{StageKey, CurrentWorkers}] = ets:lookup(erlda_workers_table, StageKey),
    MinWorkers = 1, 
    case length(CurrentWorkers) > MinWorkers of 
        true ->
            [RemovedWorker | NewWorkers] = lists:reverse(CurrentWorkers),
            exit(RemovedWorker, shutdown), 
            ets:insert(erlda_workers_table, {StageKey, NewWorkers});
        false ->
            io:format("[+][~p][~p] - Não é possível remover mais workers. Mínimo atingido ~n", [calendar:local_time(), self()])
    end.

count_workers(StageModule) ->
    StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    case ets:lookup(erlda_workers_table, StageKey) of
        [{StageKey, Workers}] -> length(Workers);
        [] -> 0
    end.
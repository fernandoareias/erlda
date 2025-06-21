-module(stage_behaviour).

-export([
    spawn_stage/1,
    loop/2,
    start_workers/3,
    select_worker/1
]).

-record(private_state, {
    processors = [] :: list(pid()),
    controller :: pid()
}).

-callback handle_command(Command :: term()) ->
    {ok, Result :: term()} | {error, Reason :: term()} |
    {forward, NextStagePid :: pid(), NewCommand :: term()}.

-callback rebalancing_policy(State :: #private_state{}) ->
    ok | {error, term()}.

-optional_callbacks([rebalancing_policy/1]).

spawn_stage(StageModule) when is_atom(StageModule) ->
    InitialState = #private_state{
        processors = []
    },
    Pid = spawn_link(?MODULE, loop, [StageModule, InitialState]),
    ControllerPid = stage_controller:spawn_controller(StageModule, Pid),
    Pid ! {set_controller, ControllerPid},
    register(StageModule, Pid),
    Pid.

loop(StageModule, State = #private_state{ processors = [] }) ->
    receive
        {set_controller, ControllerPid} ->
            link(ControllerPid),
            io:format("[+][~p][~p] - Controller configurado: ~p~n", [calendar:local_time(), self(), ControllerPid]),
            MinWorkers = 1,
            Workers = start_workers(StageModule, MinWorkers, self()),
            NewState = State#private_state{
                processors = Workers,
                controller = ControllerPid
            },
            loop(StageModule, NewState);
        {command, _, _} ->
            io:format("[/][~p][~p] - Aguardando workers ficarem prontos... ~n", [calendar:local_time(), self()]),
            loop(StageModule, State);
        stop ->
            io:format("[-][~p][~p] - Estágio parado. ~n", [calendar:local_time(), self()])
    end;
loop(StageModule, State = #private_state{ processors = _Workers }) ->
    receive 
        {add_worker, N} ->
            NewWorkers = [spawn_worker(StageModule, self()) || _ <- lists:seq(1, N)],
            io:format("[+][~p][~p] - Adicionando ~p workers para ~p~n", [calendar:local_time(), self(), N, StageModule]),
            NewState = State#private_state{
                processors = NewWorkers ++ State#private_state.processors
            },
            loop(StageModule, NewState);
        {remove_worker, N} when N > 0 ->
            {ToRemove, Remaining} = lists:split(min(N, length(State#private_state.processors)), State#private_state.processors),
            io:format("[-][~p][~p] - Removendo ~p workers de ~p~n", [calendar:local_time(), self(), length(ToRemove), StageModule]),
            lists:foreach(fun({WorkerPid, _MonitorRef}) ->
                exit(WorkerPid, kill)
            end, ToRemove),
            NewState = State#private_state{
                processors = Remaining
            },
            loop(StageModule, NewState);
        {command, Command, From} ->
            io:format("[+][~p][~p] - ~p recebeu comando de ~p ~n", [calendar:local_time(), self(), StageModule, From]),
            {WorkerPid, _MonitorRef} = pick_random_worker(State#private_state.processors),
            io:format("[+][~p][~p] - ~p recebeu comando de ~p enviando para a worker ~p ~n", [calendar:local_time(), self(), StageModule, From, WorkerPid]),
            WorkerPid ! {work, Command, From},
            loop(StageModule, State);
        {worker_ready, _WorkerPid} ->
            loop(StageModule, State);
        {'DOWN', _Ref, process, WorkerPid, Reason} ->
            io:format("[-][~p][~p] - Worker ~p morreu (~p), reiniciando...~n", [calendar:local_time(), self(), WorkerPid, Reason]),
            Workers1 = lists:keydelete(WorkerPid, 1, State#private_state.processors),
            {NewWorkerPid, NewRef} = spawn_worker(StageModule, self()),
            Workers2 = [{NewWorkerPid, NewRef} | Workers1],
            NewState = State#private_state{processors = Workers2},
            loop(StageModule, NewState);
        stop ->
            io:format("[-][~p][~p] - Estágio parado.~n", [calendar:local_time(), self()])
    end.


start_workers(StageModule, Count, StagePid) when Count > 0 ->
    [spawn_worker(StageModule, StagePid) || _ <- lists:seq(1, Count)].

spawn_worker(StageModule, StagePid) ->
    Id = erlang:unique_integer([positive]),
    Name = list_to_atom(atom_to_list(StageModule) ++ "_worker_" ++ integer_to_list(Id)),
    WorkerPid = spawn(fun() ->
        register(Name, self()),
        worker_loop(StageModule, StagePid)
    end),
    MonitorRef = erlang:monitor(process, WorkerPid),
    {WorkerPid, MonitorRef}.

worker_loop(StageModule, StagePid) ->
    StagePid ! {worker_ready, self()},
    receive
        {work, Command, From} ->
            case StageModule:handle_command(Command) of
                {ok, Result} ->
                    io:format("[+][~p][~p] - Sucesso processando o comando ~p | resultado ~p ~n", [calendar:local_time(), self(), Command, Result]);
                {error, Reason} ->
                    io:format("[-][~p][~p] - Erro motivo ~p ~n", [calendar:local_time(), self(), Reason]);
                {forward, NextStagePid, NewCommand} ->
                    NextStagePid ! {command, NewCommand, From}
            end,
            worker_loop(StageModule, StagePid)
    end.


select_worker(Workers) ->
    N = length(Workers),
    case N of
        0 ->
            erlang:error(no_workers);
        _ ->
            Index = rand:uniform(N),
            lists:nth(Index, Workers)
    end.

pick_random_worker(Workers) ->
    Tuple = list_to_tuple(Workers),
    N = tuple_size(Tuple),
    Index = rand:uniform(N),
    element(Index, Tuple).
-module(stage_behaviour).

-export([
    spawn_stage/1,
    loop/2,
    start_workers/2,
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
    StageControllerPid = stage_controller:spawn_controller(StageModule, self()),
    link(StageControllerPid),
    InitialState = #private_state{
        controller = StageControllerPid
    },
    Pid = spawn_link(?MODULE, loop, [StageModule, InitialState]),
    Pid ! initialize_workers,
    register(StageModule, Pid),
    Pid.

loop(StageModule, State = #private_state{ processors = [] }) ->
    receive
        initialize_workers ->            
            NumWorkers = 10,
            Workers = start_workers(StageModule, NumWorkers),
            loop(StageModule, State#private_state{ processors = Workers });
        {command, _, _} ->
            io:format("[/][~p][~p] - Aguardando workers ficarem prontos... ~n", [calendar:local_time(), self()]),
            loop(StageModule, State);
        stop ->
            io:format("[-][~p][~p] - Estágio parado. ~n", [calendar:local_time(), self()])
    end;

loop(StageModule, State = #private_state{ processors = _Workers }) ->
    receive
        {command, Command, From} ->
            io:format("[+][~p][~p] - ~p recebeu comando de ~p ~n", [calendar:local_time(), self(), StageModule, From]),
            {WorkerPid, _MonitorRef} = pick_random_worker(State#private_state.processors),
            io:format("[+][~p][~p] - ~p recebeu comando de ~p enviando para a worker ~p ~n", [calendar:local_time(), self(), StageModule, From, WorkerPid]),
            WorkerPid ! {work, Command, From},
            loop(StageModule, State);
        {worker_ready, WorkerPid} ->
            WorkerPid ! {continue, self()},
            loop(StageModule, State#private_state{ processors = [WorkerPid | State#private_state.processors] });
        {'DOWN', _Ref, process, WorkerPid, Reason} ->
            io:format("[-][~p][~p] - Worker ~p morreu (~p), reiniciando...~n", [calendar:local_time(), self(), WorkerPid, Reason]),
            Workers1 = lists:keydelete(WorkerPid, 1, State#private_state.processors),
            {NewWorkerPid, NewRef} = spawn_worker(StageModule),
            Workers2 = [{NewWorkerPid, NewRef} | Workers1],
            loop(StageModule, State#private_state{processors = Workers2});
        stop ->
            io:format("[-][~p][~p] - Estágio parado.~n", [calendar:local_time(), self()])
    end.


start_workers(StageModule, Count) when Count > 0 ->
    [spawn_worker(StageModule) || _ <- lists:seq(1, Count)].

spawn_worker(StageModule) ->
    Id = erlang:unique_integer([positive]),
    Name = list_to_atom(atom_to_list(StageModule) ++ "_worker_" ++ integer_to_list(Id)),
    WorkerPid = spawn_link(fun() ->
        register(Name, self()),
        worker_loop(StageModule)
    end),
    MonitorRef = erlang:monitor(process, WorkerPid),
    {WorkerPid, MonitorRef}.
    
worker_loop(StageModule) ->
    receive
        {work, Command, From} ->
            case StageModule:handle_command(Command) of
                {ok, Result} ->
                    io:format("[+][~p][~p] - Sucesso processando o comando ~p | resultado ~p ~n", [calendar:local_time(), self(), Command, Result]);
                    % From ! {reply, Result};
                {error, Reason} ->
                    io:format("[-][~p][~p] - Erro motivo ~p ~n", [calendar:local_time(), self(), Reason]);
                    % From ! {error, Reason};
                {forward, NextStagePid, NewCommand} ->
                    NextStagePid ! {command, NewCommand, From}
            end,
            self() ! {worker_ready, self()},
            worker_loop(StageModule);
        {continue, _StagePid} ->
            worker_loop(StageModule)
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
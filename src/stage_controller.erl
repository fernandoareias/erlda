-module(stage_controller).

-export([spawn_controller/2, loop/1]).

-record(controller_state, {
    stage_module,
    stage_pid,
    min_workers = 1,
    max_workers = 20,
    scale_up_threshold = 10,  
    scale_down_threshold = 2,
    check_interval = 5000      % 5s
}).

spawn_controller(StageModule, StagePid) ->
    State = #controller_state{stage_module = StageModule, stage_pid = StagePid},
    Pid = spawn(?MODULE, loop, [State]),
    ControllerName = list_to_atom(atom_to_list(StageModule) ++ "_controler"),
    register(ControllerName, Pid),
    Pid.

loop(State = #controller_state{check_interval = Interval}) ->
    erlang:send_after(Interval, self(), check_balance),
    NewState = receive
        check_balance ->
            check_stage_balance(State);
        stop ->
            io:format("[-][~p][~p] - Controlador de ~p parado.~n", [calendar:local_time(), self(), State#controller_state.stage_module]),
            exit(normal)
    end,
    loop(NewState).

check_stage_balance(State) ->
    #controller_state{
        stage_pid = StagePid,
        stage_module = StageModule,
        min_workers = Min,
        max_workers = Max,
        scale_up_threshold = Up,
        scale_down_threshold = Down
    } = State,
    
    case is_process_alive(StagePid) of
        true ->
            {message_queue_len, QLen} = process_info(StagePid, message_queue_len),
            NumWorkers = stage_behaviour:count_workers(StageModule),
            
            io:format("[C][~p][~p] - Analisando métricas: QLen=~p, Workers=~p, Up=~p, Down=~p~n", 
                      [calendar:local_time(), self(), QLen, NumWorkers, Up, Down]),

            if
                QLen > Up andalso NumWorkers < Max ->
                    io:format("[^][~p][~p] - Aumento workers para ~p. QLen: ~p, Workers: ~p~n", [calendar:local_time(), self(), StageModule, QLen, NumWorkers]),
                    stage_behaviour:add_worker(StageModule, self());
                
                QLen < Down andalso NumWorkers > Min ->
                    io:format("[v][~p][~p] - Diminuindo workers para ~p. QLen: ~p, Workers: ~p~n", [calendar:local_time(), self(), StageModule, QLen, NumWorkers]),
                    stage_behaviour:remove_worker(StageModule, any);
                true ->
                    io:format("[=][~p][~p] - Workers estáveis para ~p. QLen: ~p, Workers: ~p~n", [calendar:local_time(), self(), StageModule, QLen, NumWorkers])
            end;
        false ->
            io:format("[-][~p][~p] - Stage ~p não está mais vivo~n", [calendar:local_time(), self(), StageModule])
    end,
    State.
 
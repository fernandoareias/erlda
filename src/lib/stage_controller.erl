-module(stage_controller).

-export([spawn_controller/2, loop/1]).

-record(controller_state, {
    stage_module,
    stage_pid,
    min_workers = 10,
    max_workers = 200,
    scale_up_threshold = 100,  
    scale_down_threshold = 10,
    check_interval = 1000,     % 1s
    consecutive_hot = 0,
    consecutive_cold = 0,
    last_action_ms = 0,
    action_cooldown_ms = 2000,
    min_samples_to_act = 1
}).

spawn_controller(StageModule, StagePid) ->
    State = #controller_state{stage_module = StageModule, stage_pid = StagePid},
    Pid = spawn(?MODULE, loop, [State]),
    ControllerName = list_to_atom(atom_to_list(StageModule) ++ "_controller"),
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
            MaxWorkerQLen = max_worker_queue(StageModule),
            NumWorkers = stage_behaviour:count_workers(StageModule),
            
            io:format("[+][~p][~p] - Métricas: MaxWorkerQLen=~p, Workers=~p, Up=~p, Down=~p~n", 
                      [calendar:local_time(), self(), MaxWorkerQLen, NumWorkers, Up, Down]),

            NewState1 =
                if MaxWorkerQLen > Up -> incr_hot(reset_cold(State));
                   MaxWorkerQLen < Down -> incr_cold(reset_hot(State));
                   true -> reset_hot(reset_cold(State))
                end,
            NewState2 = maybe_scale(StageModule, NumWorkers, Min, Max, NewState1),
            NewState2;
        false ->
            io:format("[-][~p][~p] - Stage ~p não está mais vivo~n", [calendar:local_time(), self(), StageModule])
    end,
    State.

max_worker_queue(StageModule) ->
    StageKey = list_to_atom(atom_to_list(StageModule) ++ "_workers"),
    case ets:lookup(erlda_workers_table, StageKey) of
        [{StageKey, Workers}] when is_list(Workers), Workers =/= [] ->
            lists:max([queue_len(W) || W <- Workers]);
        _ -> 0
    end.

queue_len(Pid) when is_pid(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, N} -> N;
        _ -> 0
    end.

now_ms() -> erlang:monotonic_time(millisecond).

cooldown_over(#controller_state{ last_action_ms = T, action_cooldown_ms = C }) ->
    now_ms() - T >= C.

incr_hot(S = #controller_state{ consecutive_hot = H }) -> S#controller_state{ consecutive_hot = H + 1 }.
incr_cold(S = #controller_state{ consecutive_cold = C }) -> S#controller_state{ consecutive_cold = C + 1 }.
reset_hot(S) -> S#controller_state{ consecutive_hot = 0 }.
reset_cold(S) -> S#controller_state{ consecutive_cold = 0 }.

maybe_scale(StageModule, NumWorkers, Min, Max, S = #controller_state{ consecutive_hot = H, consecutive_cold = C, min_samples_to_act = K }) ->
    CanAct = cooldown_over(S),
    case true of
        _ when H >= K andalso NumWorkers < Max andalso CanAct ->
            io:format("[+][~p][~p] - Scale UP ~p (consecutive_hot=~p)~n", [calendar:local_time(), self(), StageModule, H]),
            stage_behaviour:add_worker(StageModule),
            S#controller_state{ consecutive_hot = 0, last_action_ms = now_ms() };
        _ when C >= K andalso NumWorkers > Min andalso CanAct ->
            io:format("[-][~p][~p] - Scale DOWN ~p (consecutive_cold=~p)~n", [calendar:local_time(), self(), StageModule, C]),
            stage_behaviour:remove_worker(StageModule, any),
            S#controller_state{ consecutive_cold = 0, last_action_ms = now_ms() };
        _ ->
            S
    end.
 
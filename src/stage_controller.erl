-module(stage_controller).

-export([spawn_controller/2, loop/2]).
 
spawn_controller(StageModule, StagePid) ->
    io:format("[+][~p][~p] - Controller -> Stage ~p | Pid ~p ~n", [calendar:local_time(), self(), StageModule, StagePid]),
    spawn_link(?MODULE, loop, [StageModule, StagePid]).

loop(StageModule, StagePid) ->    
    loop(StageModule, StagePid).
-module(myrpc).

-export([start/0, apply/2, apply/3]).

start() ->
    register(myrpc, spawn(fun rpc_server/0)).

apply(Fun, Args) ->
    worker_response(request_worker(self(), Fun, Args)).

apply(Fun, Args, Timeout) ->
    worker_response(request_worker(self(), Fun, Args, Timeout)).

request_worker(ClientPid, Fun, Args) ->
    spawn(fun() ->
        WorkerPid = self(),
        myrpc ! {WorkerPid, Fun, Args},
        receive
            X -> ClientPid ! {WorkerPid, X}
        end
    end).

request_worker(ClientPid, Fun, Args, Timeout) ->
    spawn(fun() ->
        WorkerPid = self(),
        myrpc ! {WorkerPid, Fun, Args},
        receive
            X -> ClientPid ! {WorkerPid, X}
        after Timeout -> ClientPid ! {WorkerPid, timeout}
        end
    end).

worker_response(WorkerPid) ->
    receive
        {Pid, X} when Pid == WorkerPid ->
            X
    end.

rpc_server() ->
    receive
        {Pid, Fun, Args} ->
            spawn(fun() -> Pid ! erlang:apply(Fun, Args) end),
            rpc_server();
        Other ->
            io:format("Unknown message received: ~p~n", [Other]),
            rpc_server()
    end.

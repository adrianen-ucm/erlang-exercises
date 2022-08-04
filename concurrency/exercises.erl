-module(exercises).

-export([ping/0, fibserver/0, largest_number/0, n_connected_cells/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. Write a function ping() that continously receives messages on the %
%%% format {req, Msg, Pid} and send back a reply to the process with Pid %
%%% as the message {ack, Msg}.                                           %
%%%                                                                      %
%%% Moreover, remove any message from the mailbox which does match the   %
%%% pattern {req, Msg, Pid}, and print them out as an error indication.  %
%%%                                                                      %
%%% Check with a guard if Pid is a process identifier, so that only      %
%%% tuples with a proper pid in the last argument are accepted.          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ping() ->
    receive
        {req, Msg, Pid} when is_pid(Pid) ->
            Pid ! {ack, Msg};
        Other ->
            io:format("Unknown message received: ~p~n", [Other])
    end,
    ping().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. Write a function fibserver() which continuously receives messages %
%%% on the format {fib, N, Pid}, and computes fib(N), and returns the    %
%%% answer to Pid as the message {fib, N, is, Result}.                   %
%%%                                                                      %
%%% If necessary, redesign your function fibserver() so that it can      %
%%% handle multiple messages at the same time, that is, calculating      %
%%% fib(N) for a large N should not block the server. Rather the server  %
%%% should as quickly as possible be ready to receive new orders to      %
%%% compute fibonacci numbers IN PARALLEL with the already ongoing       %
%%% request.                                                             %
%%%                                                                      %
%%% Try calling fibserver() in parallel. How do you do this best?        %
%%%                                                                      %
%%%   See the test file for a parallel test with 1000 requests.          %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This could be improved by notifying the caller process in case that
% the spawned one crashes (e.g. request with a non valid N) as we did in
% class. I have omitted that in order to keep the exercise answer simple.
fibserver() ->
    receive
        {fib, N, Pid} when is_pid(Pid) ->
            spawn(fun() -> Pid ! {fib, N, is, fib(N)} end);
        Other ->
            io:format("Unknown message received: ~p~n", [Other])
    end,
    fibserver().

% Tail recursive implementation with an auxiliary function.
% It only accepts natural numbers.
fib(N) when is_integer(N), N >= 0 ->
    fib_rec(N, 0, 1).

fib_rec(0, F0, _) ->
    F0;
fib_rec(N, F0, F1) ->
    fib_rec(N - 1, F1, F0 + F1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. Write a simple server that records the largest number received.   %
%%% Incoming messages to the server:                                     %
%%%                                                                      %
%%%   {put, N} where N is an integer, remembers N if N is bigger than    %
%%%   any number received earlier.                                       %
%%%                                                                      %
%%%   {query,Pid} sends back the largest number received so far to Pid   %
%%%   as the message {largest,V} where V is the largest value.           %
%%%                                                                      %
%%%   {statistics,Pid} sends back a tuple {P,Q} where P and Q are the    %
%%%   number of put and query messages received by the server.           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Initially, there is no largest received number (i.e. none).
largest_number() ->
    largest_number({none, 0, 0}).

largest_number(State = {Largest, Puts, Queries}) ->
    receive
        {put, N} when is_integer(N), Largest == none orelse N > Largest ->
            largest_number({N, Puts + 1, Queries});
        {put, N} when is_integer(N) ->
            largest_number({Largest, Puts + 1, Queries});
        {query, Pid} when is_pid(Pid) ->
            Pid ! {largest, Largest},
            largest_number({Largest, Puts, Queries + 1});
        {statistics, Pid} when is_pid(Pid) ->
            Pid ! {Puts, Queries},
            largest_number(State);
        Other ->
            io:format("Unknown message received: ~p~n", [Other]),
            largest_number(State)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. Write a function n_connected_cells(N, Pid) where N is a natural   %
%%% number > 0, and Pid is a process identifier, which creates a         %
%%% "linear network" of n cells (where each cell is a process):          %
%%%                                                                      %
%%% [cell_1] --> [cell_2] --> [cell_2] --> [cell_3] --> ... -> [cell_N]  %
%%%                                                                      %
%%% such that a cell i sends along any message received to the next cell %
%%% i+1.                                                                 %
%%%                                                                      %
%%% The final cell_N forwards any received message to Pid, i.e., the     %
%%% process identifier which is the second argument to the function      %
%%% n_connected_cells(N, Pid).                                           %
%%%                                                                      %
%%% The function n_connected_cells(N, Pid) should return the process     %
%%% identifier of cell_1.                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_connected_cells(N, Pid) when is_integer(N), N > 0, is_pid(Pid) ->
    n_cell(N, Pid).

% Recursive function to create the 'spine' of the network
% while spawning the cell processes.
n_cell(0, Pid) ->
    Pid;
n_cell(N, Pid) ->
    spawn(fun() ->
        io:format("Created cell ~p~n", [self()]),
        cell_loop(n_cell(N - 1, Pid))
    end).

% Each cell repeatedly forwards any received message to the next one.
cell_loop(Next) ->
    receive
        Msg ->
            io:format("Forwarding message from ~p~n", [self()]),
            Next ! Msg
    end,
    cell_loop(Next).

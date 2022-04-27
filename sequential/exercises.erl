-module(exercises).

-export([
    fib/1,
    sum/1,
    member/2,
    insert/2,
    sort/1,
    fib_with_index/1,
    keyfind/2,
    merge/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 0. Write a function fib(N) which returns the fibonacci number N.     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tail recursive implementation with an auxiliary function.
% Only accepts natural numbers as input.
fib(N) when is_integer(N), N >= 0 ->
    fib_rec(0, 1, N).

fib_rec(A, _, 0) ->
    A;
fib_rec(A, B, N) ->
    fib_rec(B, A + B, N - 1).

% The reason for the proposed results are
% > fib(2).         % 1
% > fib(0.5).       % Exception, no clause for that
% > fib(-4).        % Exception, no clause for that
% > fib([]).        % Exception, no clause for that
% > X = fib(7).     % 13 (side effect, binds it to X)
% > X.              % 13
% > X == 5.         % false
% > fib(X).         % 233
% > X = fib(7).     % 13 (computes and matches with the value of X)
% > X = fib(5).     % Exception, the result does not match the value of X

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 1. Write a function sum(L), where L is a list, to calculate the sum  %
%%% of integers in the list L.                                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tail recursive implementation with an auxiliary function.
sum(L) ->
    sum_rec(0, L).

sum_rec(A, []) ->
    A;
sum_rec(A, [H | T]) when is_integer(H) ->
    sum_rec(A + H, T);
sum_rec(A, [_ | T]) ->
    sum_rec(A, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2. Write a function member(E, L) which looks for E inside L, and     %
%%% returns true if found and false otherwise.                           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

member(_, []) ->
    false;
member(E, [E | _]) ->
    true;
member(E, [_ | T]) ->
    member(E, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 2.5 Write a function insert(I, L) which inserts an integer in        %
%%% a sorted list of integers, so that the list stays sorted.            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert(I, []) when is_integer(I) ->
    [I];
insert(I, [H | _] = L) when is_integer(I), is_integer(H), I =< H ->
    [I | L];
insert(I, [H | T]) when is_integer(I), is_integer(H) ->
    [H | insert(I, T)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 3. Write a function sort(L), which sorts the elements in L in        %
%%% ascending order.                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Naive quicksort implementation using list comprehensions.
sort([]) ->
    [];
sort([H | T]) ->
    sort([L || L <- T, L =< H]) ++ [H] ++ sort([R || R <- T, R > H]).

% What happens if you call the function with a list containing differently
% typed elements?
%
% As they are sorted using the built-in comparison operators of Erlang,
% which allow to compare elements of different types, they are sorted
% according to its rules. From the documentation:
%
% number < atom < reference < fun < port < pid < tuple < list < bit string

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 4. Write a function fib_with_index which returns both the index and  %
%%% the fibonacci number                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Using the fibonacci implementation from above.
fib_with_index(N) ->
    {N, fib(N)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 5. Write a function keyfind(Key, List), which given a list of tuples,%
%%% returns the first value that matches the Key and false if none.      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

keyfind(_, []) ->
    false;
keyfind(K, [{K, V} | _]) ->
    V;
keyfind(K, [_ | T]) ->
    keyfind(K, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 6. Write a function merge(L1, L2) that merges two list which are     %
%%% assumed to be sorted in ascending order, returning a list of         %
%%% elements in ascending order.                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge(XS, []) ->
    XS;
merge([X | XS], L = [Y | _]) when X =< Y ->
    [X | merge(XS, L)];
merge(XS, YS) ->
    merge(YS, XS).

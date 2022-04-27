-module(trees_record).

-export([tree/2, fold/3, find/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Redo the exercises on trees using node records instead of            %
%%% the tuple {node, Node, LeftChild, RightChild}, i.e., implement       %
%%% map and some variant of fold.                                        %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(node, {value, left, right}).

tree(_, void) ->
    void;
tree(
    F,
    #node{
        value = N,
        left = L,
        right = R
    }
) ->
    #node{
        value = F(N),
        left = tree(F, L),
        right = tree(F, R)
    }.

fold(_, A, void) ->
    A;
fold(
    F,
    A,
    #node{
        value = N,
        left = L,
        right = R
    }
) ->
    fold(F, fold(F, F(N, A), L), R).

find(P, L) ->
    F = fun
        (N, false) ->
            case P(N) of
                true ->
                    {ok, N};
                false ->
                    false
            end;
        (_, A) ->
            A
    end,
    fold(F, false, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Can you use records in the Erlang shell? No.                         %
%%% Why is this? They are a construct which exists at compilation time.  %
%%% How are records printed in the Erlang shell? As tuples with the      %
%%%                                    record name in the first position %
%%% Why is this? That is what they are at runtime.                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

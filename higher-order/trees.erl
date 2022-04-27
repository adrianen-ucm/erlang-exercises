-module(trees).

-export([
    tree/2,
    fold_nlr/3,
    fold_nrl/3,
    fold_lrn/3,
    fold_rln/3,
    fold_lnr/3,
    fold_rnl/3,
    find/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Work with binary trees, represented as tuples                        %
%%%     {node, Node, LeftChild, RightChild}                              %
%%% where Left and Right can be the atom void, signifying an absent      %
%%% child.                                                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Write a map for such trees, i.e tree(F, T), where F is a function    %
%%% that is applied to each node value and T is the tree.                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tree(_, void) ->
    void;
tree(F, {node, N, L, R}) ->
    {node, F(N), tree(F, L), tree(F, R)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Think about fold for trees, what are natural definitions? Implement  %
%%% some variant of fold for trees.                                      %
%%%                                                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% All the deep-first permutations of nlr (i.e. root node, left subtree,
% right subtree)
fold_nlr(_, A, void) ->
    A;
fold_nlr(F, A, {node, N, L, R}) ->
    fold_nlr(F, fold_nlr(F, F(N, A), L), R).

fold_nrl(_, A, void) ->
    A;
fold_nrl(F, A, {node, N, L, R}) ->
    fold_nrl(F, fold_nrl(F, F(N, A), R), L).

fold_lrn(_, A, void) ->
    A;
fold_lrn(F, A, {node, N, L, R}) ->
    F(N, fold_lrn(F, fold_lrn(F, A, L), R)).

fold_rln(_, A, void) ->
    A;
fold_rln(F, A, {node, N, L, R}) ->
    F(N, fold_rln(F, fold_rln(F, A, R), L)).

fold_lnr(_, A, void) ->
    A;
fold_lnr(F, A, {node, N, L, R}) ->
    fold_lnr(F, F(N, fold_lnr(F, A, L)), R).

fold_rnl(_, A, void) ->
    A;
fold_rnl(F, A, {node, N, L, R}) ->
    fold_rnl(F, F(N, fold_rnl(F, A, R)), L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implement find(Pred,L), which returns {ok, Value} if there is a node %
%%% with value Value in the tree, such that Pred(Value) == true, and     %
%%% otherwise returns false.                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Although it can be implemented with an early traversal stop, I have
% defined it in terms of one of the fold functions as an example.
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
    fold_nlr(F, false, L).

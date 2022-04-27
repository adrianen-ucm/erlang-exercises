-module(exercises_prop).

-export([
    prop_fib_is_fib/0,
    prop_sum_singleton/0,
    prop_sum_of_sums/0,
    prop_sum_heterogeneous/0,
    prop_insert_sorted/0,
    prop_sort_idempotent/0,
    prop_sort_permutation/0,
    prop_sort_sorts/0,
    prop_fib_with_index_has_index/0,
    prop_fib_with_index_has_fib/0,
    prop_merge_sorted_permutation/0
]).

-import(exercises, [fib/1, member/2, sum/1, insert/2, sort/1, fib_with_index/1, merge/2]).

-include_lib("eqc/include/eqc.hrl").

prop_fib_is_fib() ->
    ?FORALL(N, choose(2, 100), fib(N) =:= fib(N - 1) + fib(N - 2)).

prop_sum_singleton() ->
    ?FORALL(E, int(), sum([E]) =:= E).

prop_sum_of_sums() ->
    ?FORALL(
        L1,
        list(int()),
        ?FORALL(L2, list(int()), sum(L1) + sum(L2) =:= sum(lists:append(L1, L2)))
    ).

prop_sum_heterogeneous() ->
    ?FORALL(
        L,
        list(oneof([bool(), ok, int()])),
        sum(L) =:= sum(lists:filter(fun is_integer/1, L))
    ).

prop_insert_sorted() ->
    ?FORALL(
        S,
        ?LET(L, list(int()), sort(L)),
        ?FORALL(
            E,
            int(),
            insert(E, S) =:= sort([E | S])
        )
    ).

prop_sort_idempotent() ->
    ?FORALL(
        L,
        list(int()),
        sort(L) =:= sort(sort(L))
    ).

prop_sort_permutation() ->
    ?FORALL(
        L,
        list(int()),
        lists:subtract(L, sort(L)) =:= lists:subtract(sort(L), L)
    ).

prop_sort_sorts() ->
    ?FORALL(
        L,
        non_empty(list(int())),
        ?FORALL(
            {I, J},
            {choose(1, length(L)), choose(1, length(L))},
            ?IMPLIES(I =< J, lists:nth(I, sort(L)) =< lists:nth(J, sort(L)))
        )
    ).

prop_fib_with_index_has_index() ->
    ?FORALL(N, choose(2, 100), element(1, fib_with_index(N)) =:= N).

prop_fib_with_index_has_fib() ->
    ?FORALL(N, choose(2, 100), element(2, fib_with_index(N)) =:= fib(N - 1) + fib(N - 2)).

prop_merge_sorted_permutation() ->
    ?FORALL(
        S1,
        ?LET(L, list(int()), sort(L)),
        ?FORALL(
            S2,
            ?LET(L, list(int()), sort(L)),
            merge(S1, S2) =:= sort(lists:append(S1, S2))
        )
    ).

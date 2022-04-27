-module(my_lists_prop).

-export([prop_foldl_reverse_list/0, prop_foldr_same_list/0]).

-import(my_lists, [foldl/3, foldr/3]).

-include_lib("eqc/include/eqc.hrl").

prop_foldl_reverse_list() ->
    ?FORALL(
        L,
        list(oneof([ok, int(), bool()])),
        foldl(fun(E, A) -> [E | A] end, [], L) =:= lists:reverse(L)
    ).

prop_foldr_same_list() ->
    ?FORALL(
        L,
        list(oneof([ok, int(), bool()])),
        foldr(fun(E, A) -> [E | A] end, [], L) =:= L
    ).

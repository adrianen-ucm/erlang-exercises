-module(d_lists_prop).

-export([prop_cons_d_list/0, prop_snoc_d_list/0, prop_append_d_lists/0, prop_concat_d_lists/0]).

-import(d_lists, [cons/2, snoc/2, append/2, concat/1, to_list/1, from_list/1]).

-include_lib("eqc/include/eqc.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The dlist properties are tested against the counterpart operations   %
%%% for Erlang lists.                                                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prop_cons_d_list() ->
    ?FORALL(
        L,
        list(oneof([ok, int(), bool()])),
        ?FORALL(
            E,
            oneof([ok, int(), bool()]),
            to_list(cons(E, from_list(L))) =:= [E | L]
        )
    ).

prop_snoc_d_list() ->
    ?FORALL(
        L,
        list(oneof([ok, int(), bool()])),
        ?FORALL(
            E,
            oneof([ok, int(), bool()]),
            to_list(snoc(E, from_list(L))) =:= lists:append(L, [E])
        )
    ).

prop_append_d_lists() ->
    ?FORALL(
        L1,
        list(oneof([ok, int(), bool()])),
        ?FORALL(
            L2,
            list(oneof([ok, int(), bool()])),
            to_list(append(from_list(L1), from_list(L2))) =:= lists:append(L1, L2)
        )
    ).

prop_concat_d_lists() ->
    ?FORALL(
        LS,
        list(list(oneof([ok, int(), bool()]))),
        to_list(concat(lists:map(fun d_lists:from_list/1, LS))) =:= lists:concat(LS)
    ).

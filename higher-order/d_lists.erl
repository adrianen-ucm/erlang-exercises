-module(d_lists).

-export([new/0, cons/2, snoc/2, append/2, concat/1, from_list/1, to_list/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Your own exercises                                                   %
%%%                                                                      %
%%% I have chosen a 'dlist' implementation as an exercise on             %
%%% higher-order functions because it is a list data structure           %
%%% implementation in which the stored data is in fact a function, and   %
%%% its operations are function compositions.                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% A dlist is a function that expects a list and returns it transformed,
% so the starting point is the identity function.
new() ->
    fun(L) -> L end.

% We can add an element in the leftmost position of a dlist by wrapping
% it as follows.
cons(E, D) ->
    fun(L) -> [E | D(L)] end.

% We can also add an element in the righttmost position of a dlist by
% wrapping it as follows.
snoc(E, D) ->
    fun(L) -> D([E | L]) end.

% The same idea can be used in order to append two dlists.
append(D1, D2) ->
    fun(L) -> D1(D2(L)) end.

% And we can apply append repeatedly from the right in order to concat
% several dlists.
concat(DS) ->
    lists:foldr(fun append/2, new(), DS).

% The following two functions allow to transform between Erlang lists and
% dlists.
from_list(L1) ->
    fun(L2) -> L1 ++ L2 end.

to_list(D) ->
    D([]).

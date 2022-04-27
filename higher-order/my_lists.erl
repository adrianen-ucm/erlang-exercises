-module(my_lists).

-export([all/2, foldl/3, foldr/3, filter/2, map/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implement all(F, L) that returns true if applying the function F to  %
%%% all elements in L returns true.                                      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all(_, []) ->
    true;
all(F, [H | T]) ->
    F(H) andalso all(F, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implement foldl and foldr                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
foldl(_, A, []) ->
    A;
foldl(F, A, [H | T]) ->
    foldl(F, F(H, A), T).

foldr(_, A, []) ->
    A;
foldr(F, A, [H | T]) ->
    F(H, foldr(F, A, T)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implement filter and map.                                            %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
filter(_, []) ->
    [];
filter(F, [H | T]) ->
    case F(H) of
        true ->
            [H | filter(F, T)];
        false ->
            filter(F, T)
    end.

map(_, []) ->
    [];
map(F, [H | T]) ->
    [F(H) | map(F, T)].

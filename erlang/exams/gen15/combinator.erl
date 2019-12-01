-module(combinator).
-export([start/2, build_combinations/1]).


wait_columns(N) ->
    receive
        {gen, _, Column, Result} when N =:= 1 ->
            [{Column, Result}];
        {gen, _, Column, Result} ->
            [{Column, Result} | wait_columns(N-1)]
    end.

build_combinations([[]|_]) -> [];
build_combinations(Columns) ->
    Combination = lists:foldr(fun (X, Acc) -> 
        [X | Acc]
    end, [], lists:map(fun hd/1, Columns)),
    Other = build_combinations(lists:map(fun tl/1, Columns)),
    [Combination | Other].

%% N sono gli elementi [1 .. N]
%% M sono quelli da prendere 
start(M, N) -> 
    Elements = lists:seq(1, N),
    lists:foreach(fun (Column) ->
        P = spawn(fun () -> generator:start() end),
        P ! {gen, self(), Elements, M, Column}
    end, Elements),
    Columns = lists:sort(fun ({Id1, _}, {Id2, _}) ->
        Id1 > Id2
    end, wait_columns(N)),
    SortedColumns = lists:map(fun ({_, X}) ->
        X
    end, Columns),
    build_combinations(SortedColumns).

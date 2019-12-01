-module(generator).
-export([start/0]).


repeat(_, 0) -> [];
repeat(X, N) -> [X | repeat(X, N-1)].

sequence(Elements, N) ->
    lists:foldl(fun (X, Acc) ->
        Acc ++ repeat(X, N)
    end, [], Elements).

column(_, _, N) when N =:= 0 -> [];
column(Elements, Repeat, N) ->
    sequence(Elements, Repeat) ++ column(Elements, Repeat, N-1).

start() ->
    receive
        {gen, From, Elements, M, Column} ->
            N = length(Elements),
            RepeatSeq = trunc(math:pow(N, M - Column)),
            Repeat = trunc(math:pow(N, Column-1)),
            Result = column(Elements, Repeat, RepeatSeq),
            From ! {gen, self(), Column, Result};
        {stop} -> exit(stop_message_received)
    end,
    start().
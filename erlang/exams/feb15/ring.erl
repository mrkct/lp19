-module(ring).
-export([start/2, send_message/1, send_message/2, stop/0]).


start(Actors, Functions) -> 
    register(ring, create_ring(Actors, Functions)).

send_message(X) -> send_message(X, 1).
send_message(X, N) -> ring ! {calculate, X, N}.
stop() -> ring ! {stop}.

create_ring(N, [H|Functions]) when N > 1 ->
    Ring = create_ring(N-1, Functions),
    First = spawn(fun () ->
        link(Ring), 
        actor_loop(H, fun ({X, Loops}) -> Ring ! {calculate, X, Loops} end)
    end),
    First;
create_ring(N, [Function]) when N =:= 1 ->
    spawn(fun () ->
        actor_loop(Function, fun ({X, Loops}) ->
            if
                Loops =< 1 -> io:format("~p~n", [X]);
                Loops >  1 -> ring ! {calculate, X, Loops-1}
            end 
        end)
    end).

actor_loop(Function, DoNext) ->
    receive
        {calculate, X, Loops} -> 
            Result = Function(X),
            DoNext({Result, Loops});
        {stop} -> exit(exit_message_received)
    end,
    actor_loop(Function, DoNext).
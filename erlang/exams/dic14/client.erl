-module(client).
-export([is_prime/1, close/0]).
-define(CONTROLLER_HOST, 'controller@marco-linux').


is_prime(X) ->
    send_controller({new, X, self()}),
    receive
        {result, value_too_big} -> 
            io:format("~p is uncheckable, too big value.~n", [X]);
        {result, Result} ->
            io:format("Is ~p prime? ~p~n", [X, Result])
    after 5000 ->
        io:format("timeout~n")
    end.

close() -> send_controller({quit}).

send_controller(X) -> {controller, ?CONTROLLER_HOST} ! X.
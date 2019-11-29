-module(controller).
-export([start/1]).
-define(CLIENT, 'client@marco-linux').


start(N) ->
    ControllerPID = spawn(fun () -> 
        Ring = sieve:create_ring(self(), N),
        controller_loop(Ring, N * N)
    end),
    register(controller, ControllerPID).

controller_loop(Ring, MaxValue) ->
    receive
        {new, X, From} when X < MaxValue ->
            io:format("You asked for: ~p~n", [X]),
            Ring ! {new, X},
            receive
                {res, Result} -> From ! {result, Result}
            end;
        {new, X, From} ->
            io:format("~p is too big to be evaluated (max is ~p)~n", [X, MaxValue]),
            From ! {result, value_too_big};
        {quit} -> 
            io:format("I'm closing...~n"),
            Ring ! {quit}, 
            exit(quit_message)
    end,
    controller_loop(Ring, MaxValue).
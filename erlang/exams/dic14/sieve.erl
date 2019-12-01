-module(sieve).
-export([sieve_loop/3, create_ring/2]).


create_ring(Controller, N) ->
    First = spawn(fun () ->
        sieve_loop(1, Controller)
    end),
    PIDs = [First | lists:map(fun (Id) ->
        spawn(fun () -> sieve_loop(Id, First) end)
    end, lists:seq(2, N))],
    lists:foreach(fun ({From, To}) ->
        From ! {connect, To}
    end, lists:zip(PIDs, tl(PIDs) ++ [hd(PIDs)])),
    First.

sieve_loop(Id, SendResult) ->
    receive
        {connect, Next} ->
            link(Next),
            sieve_loop(Id, Next, SendResult)
    end.

sieve_loop(Id, Next, SendResult) ->
    receive
        {quit} -> exit(quit_message);
        {new, N} ->
            Next ! {pass, N},
            receive
                {pass, _X} -> SendResult ! {res, true};
                {res, X} -> SendResult ! {res, X}
            end;
        {pass, N} when Id < N ->
            if
                N rem Id =:= 0 ->
                    SendResult ! {res, false};
                N rem Id =/= 0 ->
                    Next ! {pass, N}
            end;
        {pass, _N} -> 
            SendResult ! {res, true}
    end,
    sieve_loop(Id, Next, SendResult).
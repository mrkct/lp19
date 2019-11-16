-module(ring2).
-export([create/2, send/2, info/1, destroy/1]).


%% Nota: questo è lo stesso esercizio in `ring.erl`, solo che è implementato 
%% in modo che sono i processi stessi a creare e collegare gli altri. La 
%% variabile Messages che viene passata sempre è inutile in verità, questo 
%% perchè non ho voglia di implementare anche quello visto che mi sembra strano
%% I messaggi hanno anche un TimeToLive, stile protocollo IP. Se non tutti i 
%% processi ricevono il messaggio forse è impostato troppo basso. 
%% Non ho implementato un controllo per fermare un messaggio dall'andare in 
%% loop, non ne avevo voglia. Guarda l'altro, li l'ho fatto

create(Processes, Messages) ->
    First = spawn(fun () -> ring_item(Messages, null) end),
    First ! {create, Processes-1, self()},
    receive
        {done, Last} -> 
            First ! {connect, Last},
            First
    end.

ring_item(Messages, Next) ->
    This = self(),
    receive
        {connect, PID} -> 
            ring_item(Messages, PID);
        {create, Processes, Callback} when Processes =:= 0 ->
            Callback ! {done, This},
            ring_item(Messages, Next);
        {create, Processes, Callback} ->
            Child = spawn(fun () -> ring_item(Messages, This) end),
            Child ! {create, Processes-1, Callback},
            ring_item(Messages, Next);
        {exit} ->
            Next ! {exit},
            io:format("[~p] Exit ~n", [This]);
        {transmit, _} when Next =:= null ->
            io:format("ERROR: [~p] I am not connected to anything yet!~n", [This]),
            ring_item(Messages, Next);
        {transmit, {From, TTL, "info"}} when TTL > 0->
            io:format("[~p] Connesso a ~p ~n", [This, Next]),
            Next ! {transmit, {From, TTL-1, "info"}},
            ring_item(Messages, Next);
        {transmit, {From, TTL, Data}} when TTL > 0 ->
            io:format("[~p]: Received '~p' ~n", [This, Data]),
            Next ! {transmit, {From, TTL-1, Data}},
            ring_item(Messages, Next);
        {transmit, {_From, TTL, _Data}} when TTL =:= 0 ->
            ring_item(Messages, Next);
        Unknown ->
            io:format("[~p]: Unknown message ~p ~n", [This, Unknown]),
            ring_item(Messages, Next)
    end.

send(Ring, Message) -> Ring ! {transmit, {self(), 200, Message}}.
info(Ring) -> send(Ring, "info").
destroy(Ring) -> Ring ! {exit}.
-module(ms).
-export([start/1, to_slave/2]).


start(N) -> 
    Slaves = create_slaves(N, []),
    register(master, spawn(fun () -> create_master(Slaves) end)).

create_slaves(Slaves, Acc) when Slaves > 0 ->
    SlavePID = spawn(fun () -> slave() end),
    create_slaves(Slaves-1, [SlavePID | Acc]);
create_slaves(Slaves, Acc) when Slaves =< 0 ->
    Acc.

to_slave(Message, N) when is_integer(N) ->
    master ! {relay, self(), N, Message}.

slave() ->
    receive
        exit -> exit(exitMessageReceived);
        X ->
            io:format("I am ~p and i received ~p~n", [self(), X]),
            slave()
    end.

create_master(Slaves) ->
    process_flag(trap_exit, true),
    lists:foreach(fun (S) ->
        link(S)
    end, Slaves),
    master(Slaves).

master(Slaves) ->
    receive
        {relay, From, To, Message} ->
            lists:nth(To, Slaves) ! Message,
            From ! ok,
            master(Slaves);
        {'EXIT', Pid, Why} ->
            io:format("[Master] process ~p died because ~p~n", [Pid, Why]),
            io:format("[Master] restarting ~p...~n", [Pid]),    
            UpdatedSlaves = list_substitute(Slaves, Pid, spawn(fun () ->
                slave()
            end)),
            master(UpdatedSlaves);
        {status} ->
            io:format("There are ~p slaves~n~p~n", [length(Slaves), Slaves]);
        Any ->
            io:format("[Master] Unknown message ~p~n", [Any]),
            master(Slaves)
    end.

%% Sostituisce un elemento nella lista con un'altro
%% Se non è presente non fa niente
list_substitute([Was | Tail], Was, Into) -> [Into | Tail];
list_substitute([H | Tail], Was, Into) ->
    [H | list_substitute(Tail, Was, Into)].
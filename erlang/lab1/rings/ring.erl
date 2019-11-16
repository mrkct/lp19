-module(ring).
-export([start/2, send/2]).


%% Exercise 3: The Process Ring.
%% 
%% Write a program that will create N processes connected in a ring. 
%% Once started, these processes will send M number of messages around the ring 
%% and then terminate gracefully when they receive a quit message. 
%% You can start the ring with the call ring:start(M, N, Message).
%% 
%% There are two basic strategies to tackling this exercise. 
%% The first one is to have a central process that sets up the ring and initiates 
%% sending the message. 
%% The second strategy consists of the new process spawning the next process in 
%% the ring. 
%% With this strategy, you have to find a method to connect the first process 
%% to the second process.
%% 
%% Try to solve the exercise in both manners. 
%% Note, when writing your program, make sure your code has many io:format 
%% statements in every loop iteration; this will give you a complete overview 
%% of what is happening (or not happening) and should help you solve the exercise.

start(HowMany, Processes) ->
    PidList = create_and_connect(Processes, HowMany),
    [First | _] = PidList,
    [Last | _] = lists:reverse(PidList),
    Last ! {null, {connect, First}},
    First.

send(Ring, Message) ->
    Ring ! {send, {transmit, {Ring, Ring, Message}}}.

create_and_connect(Processes, HowMany) ->
    if
        Processes > 1 ->
            Pid = spawn(fun() -> ring_item(HowMany) end),
            [Next | Tail] = create_and_connect(Processes-1, HowMany),
            Pid ! {null, {connect, Next}},
            [Pid, Next | Tail];
        Processes =:= 1 ->
            Pid = spawn(fun() -> ring_item(HowMany) end),
            [Pid];
        Processes =< 0 -> []
    end.

ring_item(HowMany) ->
    receive
        {_, {connect, To}} -> ring_item(HowMany, To);
        _ ->
            io:format("ERROR: Non sono ancora connesso a niente(~p)~n", [self()])
    end.

ring_item(HowMany, Next) ->
    receive 
        {send, Message} ->
            Next ! Message;
        {transmit, {Sender, Node, Message}} when Sender =/= self() ->
            handle_message({Sender, Node, Message}, HowMany, Next),
            if
                HowMany > 0 -> 
                    Next ! {transmit, {Sender, self(), Message}},
                    ring_item(HowMany-1, Next);
                true ->
                    io:format("Sono ~p ed ho finito~n", [self()])
            end;
        {exit} ->
            io:format("[~p] Exit~n", [self()]),
            Next ! {exit};
        X ->
            io:format("[~p] wtf: ~p~n", [self(), X]),
            ring_item(HowMany, Next)
    end.

handle_message(Message, _HowMany, _Next) ->
    {_Sender, Node, Data} = Message,
    io:format("Sono ~p ed ho ricevuto ~p da ~p.~n", [self(), Data, Node]).
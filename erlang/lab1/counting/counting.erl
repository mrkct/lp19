-module(counting).
-export([start/0, stop/0, add/2, multiply/2, print/1, status/0]).


%% Write a module counting which provides the functionality for interacting 
%% with a server that counts how many times its services has been requested.
%% 
%% It has to implement several services dummy1, ... dummyn 
%% (doesn't matter what they do or their real interface) and a service tot 
%% that returns a list of records indexed on each service (tot included) 
%% containing also how many times such a service has been requested. 
%% Test it from the shell

start() ->
    ServerPID = spawn(fun () -> counting_loop([]) end),
    register(server, ServerPID).

stop() ->
    server ! {self, make_ref(), shutdown},
    unregister(server).

add(X, Y) ->
    Ref = make_ref(),
    ServerPID = whereis(server),
    server ! {self(), Ref, service, add, {X, Y}},
    receive
        {ServerPID, Ref, Result} -> Result;
        Any -> io:format("[Add]: Something went wrong: ~p ~n", [Any])
    end.

multiply(X, Y) ->
    Ref = make_ref(),
    ServerPID = whereis(server),
    server ! {self(), Ref, service, multiply, {X, Y}},
    receive
        {ServerPID, Ref, Result} -> Result;
        Any -> io:format("[Multiply]: Something went wrong: ~p ~n", [Any])
    end.

print(Message) ->
    Ref = make_ref(),
    ServerPID = whereis(server),
    server ! {self(), Ref, service, print, Message},
    receive
        {ServerPID, Ref, ok} -> ok;
        Any -> io:format("[Print]: Something went wrong: ~p ~n", [Any])
    end.

status() ->
    Ref = make_ref(),
    ServerPID = whereis(server),
    server ! {self(), Ref, service, status, null},
    receive
        {ServerPID, Ref, ok} -> ok;
        Any -> io:format("[Status]: Something went wrong: ~p ~n", [Any])
    end.

increment(Service, List) ->
    case List of
        [] -> [{Service, 1}];
        [{Service, X}|Tail] -> [{Service, X+1}|Tail];
        [X|Tail] -> [X|increment(Service, Tail)]
    end.

counting_loop(Counters) ->
    receive
        {Sender, Ref, service, Service, Extra} ->
            UpdatedCounters = increment(Service, Counters),
            case Service of
                add ->
                    {X, Y} = Extra,
                    Sender ! {self(), Ref, X + Y},
                    counting_loop(UpdatedCounters);
                multiply ->
                    {X, Y} = Extra,
                    Sender ! {self(), Ref, X * Y},
                    counting_loop(UpdatedCounters);
                print ->
                    io:format("~p~n", [Extra]),
                    Sender ! {self(), Ref, ok},
                    counting_loop(UpdatedCounters);
                status ->
                    io:format("~p is running. These are the counters: ~n", [self()]),
                    lists:foreach(fun ({S, C}) -> 
                        io:format("~p : ~p ~n", [S, C]) 
                    end, UpdatedCounters),
                    Sender ! {self(), Ref, ok},
                    counting_loop(UpdatedCounters)
            end;
        {Sender, Ref, shutdown} ->
            io:format("~p shutting down...", [self()]),
            Sender ! {self(), Ref, shutting_down};
        Any ->
            io:format("~p unrecognized message ~p ~n", [self(), Any]),
            counting_loop(Counters)
    end.

-module(drs).
-export([start/0, reverse/1]).


reverse(S) -> 
    MasterPID = whereis(master),
    Ref = make_ref(),
    master ! {reverse, self(), Ref, S},
    receive
        {reverse, MasterPID, Ref, Result} -> Result;
        Any -> io:format("weird message ~p~n", [Any])
    after 5000 ->
        io:format("master timed out~n")
    end.


start() -> register(master, spawn(?MODULE, master_loop, [])).

slave(Id) ->
    receive
        {reverse, From, X} ->
            From ! {reverse, Id, string:reverse(X)};
        Any ->
            io:format("[~p] bad message ~p~n", [self(), Any])
    end.

master_loop() ->
    receive
        {reverse, From, Ref, String} ->
            % Here we split the string and create a slave for each part
            SubStrings = split_string(String),    
            IndexedSubStrings = lists:zip(
                SubStrings, 
                lists:seq(1, length(SubStrings))
            ),
            lists:foreach(fun ({S, Index}) -> 
                Slave = spawn(fun () -> slave(Index) end),
                Slave ! {reverse, self(), S}
            end, IndexedSubStrings),
            
            % Here we read all responses and sort them based on their id
            Responses = lists:sort(fun ({reverse, Id1, _}, {reverse, Id2, _}) ->
                Id1 > Id2
            end, receive_responses(length(SubStrings),[])),
            
            % We attach them in the opposite order and get the reversed string
            Reversed = lists:foldr(fun ({_, _, S}, Acc) ->
                S ++ Acc
            end, "", Responses),
            From ! {reverse, self(), Ref, Reversed},
            master_loop();
        Any ->
            io:format("[Master] Unknown message ~p~n", [Any]),
            master_loop()
    end.

receive_responses(HowMany, Acc) when HowMany =:= 0 -> Acc;
receive_responses(HowMany, Acc) when HowMany > 0 ->
    receive
        {reverse, Id, S} -> receive_responses(HowMany-1, [{reverse, Id, S} | Acc]);
        Any -> io:format("weird message in receive_responses[~p]~n", [Any])
    end.

%% Separa una stringa in blocchi da 10 caratteri max
split_string(S) when length(S) =< 10 ->
    [S];
split_string(S) ->
    [string:slice(S, 0, 10) | split_string(string:slice(S, 10, infinity))].
-module(hypercube).
-export([start/0, hamilton/2, gray/1]).

start() -> 
    Labels = [
        "0000", "0001", "0010", "0011", 
        "0100", "0101", "0110", "0111",
        "1000", "1001", "1010", "1011", 
        "1100", "1101", "1110", "1111"
    ],
    Vertices = lists:map(fun (Label) ->
        Pid = spawn(fun () -> vertex_loop(Label, []) end),
        {Label, Pid}
    end, Labels),
    
    lists:foreach(fun ({_, Vertex}) ->
        lists:foreach(fun ({Label, Pid}) -> 
            Vertex ! {connect, Label, Pid}
        end, Vertices)
    end, Vertices),
    [{_, First} | _] = Vertices,
    register(client, self()),
    register(hypercube, First).

hamilton(Message, ["0000" | Path]) -> 
    hypercube ! {msg, Message, Path},
    receive
        {msg, X} -> X;
        {cant_send, Id, X} ->
            io:format("message couldnt continue after ~p. this was the path: ~p~n", [Id, X]),
            X
    end.
gray(0) -> [""];
gray(N) ->
    Gray = gray(N-1),
    ["0" ++ X || X <- Gray] ++ ["1" ++ X || X <- lists:reverse(Gray)].

differences([X|A], [Y|B]) ->
    if
        X =:= Y -> differences(A, B);
        X =/= Y -> 1 + differences(A, B)
end;
differences([], []) -> 0.

find_connection(_, []) -> {none};
find_connection(Label, [{Label, Pid}|_]) -> {some, {Label, Pid}};
find_connection(Label, [{_, _} | Rest]) -> find_connection(Label, Rest).

vertex_loop(Id, Connections) ->
    receive
        {connect, Label, Pid} ->
            UpdatedConnections = case differences(Label, Id) of
                1 -> 
                    link(Pid),
                    [{Label, Pid} | Connections];
                _ -> Connections
            end,
            vertex_loop(Id, UpdatedConnections);
        {msg, X, []} -> 
            client ! {msg, {src, Id, X}},
            vertex_loop(Id, Connections);
        {msg, X, [Next|Path]} ->
            case find_connection(Next, Connections) of
                {some, {_Label, Pid}} ->
                    Pid ! {msg, {src, Id, X}, Path};
                {none} ->
                    client ! {cant_send, Id, X}
            end,
            vertex_loop(Id, Connections)
    end.
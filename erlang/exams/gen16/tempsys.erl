-module(tempsys).
-export([startsys/0]).


startsys() -> 
    FromCelsiusFunctions = [
        {celsius, fun (X) -> X end},
        {fahrenheit, fun (X) -> X * 9/5 + 32 end},
        {kelvin, fun (X) -> X + 273.15 end},
        {rankine, fun (X) -> (X + 273.15) * 9/5 end},
        {delisle, fun (X) -> (100 - X) * 3/2 end},
        {newton, fun (X) -> X * 33/100 end},
        {reamur, fun (X) -> X * 4/5 end},
        {romer, fun (X) -> X * 21/40 + 7.5 end}
    ],
    FromCelsiusNodes = lists:map(fun ({Scale, F}) -> 
        Pid = spawn(fun () -> from_celsius_node(Scale, F) end),
        {Scale, Pid}
    end, FromCelsiusFunctions),
    ToCelsiusFunctions = [
        {celsius, fun (X) -> X end},
        {fahrenheit, fun (X) -> (X - 32) * 5/9 end},
        {kelvin, fun (X) -> X - 273.15 end},
        {rankine, fun (X) -> X * 5/9 - 273.15 end},
        {delisle, fun (X) -> X * 2/3 + 100 end},
        {newton, fun (X) -> X * 100/33 end},
        {reamur, fun (X) -> X * 5/4 end},
        {romer, fun (X) -> (X - 7.5) * 40/21 end}
    ],
    ToCelsiusNodes = lists:map(fun ({Scale, F}) -> 
        Pid = spawn(fun () -> to_celsius_node(FromCelsiusNodes, Scale, F) end),
        {Scale, Pid}
    end, ToCelsiusFunctions),
    register(tempsys, spawn(fun () -> tempsys_client(ToCelsiusNodes) end)).


find_node([], _) -> pass;
find_node([{Label, Node}|_], Label) -> Node;
find_node([_|Tail], Label) -> find_node(Tail, Label).

from_celsius_node(MyScale, F) ->
    receive
        {convert, Sender, {from, 'celsius', to, MyScale, X}} ->
            Sender ! {result, F(X)};
        {convert, _Sender, {from, A, to, B, X}} ->
            io:format("Sono [celsius->~p] ma hai chiesto di convertire da ~p->~p (~p)~n", [MyScale, A, B, X]);
        Any ->
            io:format("bad message (~p)~n", [Any])
    end,
    from_celsius_node(MyScale, F).

to_celsius_node(FromCelsiusNodes, MyScale, F) ->
    receive
        {convert, Sender, {from, MyScale, to, To, X}} ->
            Celsius = F(X),
            N = find_node(FromCelsiusNodes, To),
            N ! {convert, self(), {from, 'celsius', to, To, Celsius}},
            receive
                Any -> Sender ! Any
            end
    end,
    to_celsius_node(FromCelsiusNodes, MyScale, F).

tempsys_client(ToCelsiusNodes) ->
    receive
        {convert, Sender, {from, From, to, To, X}} ->
            N = find_node(ToCelsiusNodes, From),
            N ! {convert, self(), {from, From, to, To, X}},
            receive
                Any -> Sender ! Any
            end
    end,
    tempsys_client(ToCelsiusNodes).

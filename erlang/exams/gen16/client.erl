-module(client).
-export([convert/5]).


convert(from, From, to, To, Value) -> 
    tempsys ! {convert, self(), {from, From, to, To, Value}},
    receive
        {result, X} -> X;
        Any -> io:format("fail: ~p~n", [Any])
    after 2000 ->
        io:format("client timeout~n")
end.
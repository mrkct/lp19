-module(echoclient).
-export([create/0, exit/0]).



create() ->
    register(client, spawn(fun () -> client_loop() end)),
    link(whereis(server)).

exit() -> client ! {shutdown}.

client_loop() ->
    receive
        {shutdown} -> exit(shutdown);
        Any -> 
            io:format("Client riceve ~p ~n", [Any]),
            client_loop()
    end.
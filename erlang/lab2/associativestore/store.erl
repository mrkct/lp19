-module(store).
-export([create/1, store/3, lookup/2, server_lookup/2, server_store/3]).


create(Hosts) -> 
	lists:map(fun (H) ->
		net_kernel:connect_node(H),
		case net_kernel:connect_node(H) of
			true ->
				spawn(H, fun () ->
					store_loop([])
				end);
			false ->
				io:format("Failed to connect to ~p~n", [H])
		end
	end, Hosts).
	

choose_node(Nodes) ->
	lists:nth(rand:uniform(length(Nodes)), Nodes).

lookup(Nodes, Key) ->
	Where = choose_node(Nodes),
    Ref = make_ref(),
    Where ! {self(), Ref, {lookup, Key}},
    receive
        {Where, Ref, {lookup_ok, Response}} -> Response;
        Any -> 
            io:format("[Client lookup ~p] bad response ~p ~n", [self(), Any])
    after 5000 ->
        io:format("[Client lookup ~p] Time out for lookup(~p, ~p)~n", [self(), Where, Key])
    end.

store(Nodes, Key, Value) ->
	lists:foreach(fun (Where) ->
		Ref = make_ref(),
		Where ! {self(), Ref, {store, Key, Value}},
		receive
			{Where, Ref, {store_ok}} -> ok;
			Any -> 
				io:format("[Client store ~p] bad response ~p ~n", [self(), Any])
		after 5000 ->
			io:format("[Client store ~p] Time out for store(~p, ~p, ~p)~n", [self(), Where, Key, Value])
		end
	end, Nodes).
        

server_lookup(Content, Key) ->
    case Content of
        [] -> {none};
        [{Key, Value} | _Tail] -> {some, Value};
        [_ | Tail] -> server_lookup(Tail, Key)
    end.

server_store(Content, Key, Value) ->
    case Content of
        [] -> [{Key, Value}];
        [{Key, _Val} | Tail] -> [{Key, Value} | Tail];
        [Head | Tail] -> [Head | server_store(Tail, Key, Value)]
    end.

store_loop(Content) ->
    receive
        {Sender, Ref, Any} ->
            case Any of
                {lookup, Key} ->
                    Response = server_lookup(Content, Key),
                    Sender ! {self(), Ref, {lookup_ok, Response}},
                    store_loop(Content);
                {store, Key, Value} ->
                    NewContent = server_store(Content, Key, Value),
                    Sender ! {self(), Ref, {store_ok}},
                    store_loop(NewContent);
                Any -> io:format("bad arg in Any~n")
            end;
        Any ->
            io:format("[Store ~p] bad message format ~p ~n", [self(), Any]),
            store_loop(Content)
    end.

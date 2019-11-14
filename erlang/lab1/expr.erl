-module(expr).
-compile(export_all).

stack_to_string([]) -> "";
stack_to_string([H|S]) -> " " ++ H ++ stack_to_string(S).

shunting_yard(Tokens, Stack, OutputString) -> 
    case Tokens of
        [] -> OutputString ++ stack_to_string(Stack);
        [H|Rest] -> handle_token(H, Rest, Stack, OutputString);
        [H]      -> handle_token(H, [], Stack, OutputString)
    end.

handle_token(Token, Rest, Stack, OutputString) ->
    case token:is_number(Token) of
        true -> shunting_yard(Rest, Stack, OutputString ++ " " ++ Token);
        false ->
            case token:is_operator(Token) of
                true -> handle_operator(Token, Rest, Stack, OutputString);
                false ->
                    case token:is_parenthesis(Token) of
                        true -> handle_parenthesis(Token, Rest, Stack, OutputString);
                        false -> error(unrecognised_token)
                    end
            end
    end.

handle_operator(Token, Rest, Stack, OutputString) ->
    {NewOutputString, NewStack} = case Stack of
        [] -> 
            {OutputString, [Token|Stack]};
        [Top|Tail] -> 
            case token:precedence(Top) < token:precedence(Token) of
                true  -> {OutputString, [Token|Stack]};
                false -> {OutputString ++ " " ++ Top, [Token|Tail]}
            end
    end,
    shunting_yard(Rest, NewStack, NewOutputString).

handle_parenthesis(Token, Rest, Stack, OutputString) ->
    error(unimplemented).
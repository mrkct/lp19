-module(token).
-export([precedence/1, left_assoc/1, tokenize/1]).


left_assoc(S) -> S =:= "~~".

precedence(Tok) ->
    case Tok of
        "+" -> 2;
        "-" -> 2;
        "*" -> 3;
        "/" -> 3;
        "(" -> 100;
        ")" -> 100
    end.

token_is_number(S) -> 
    case re:run(S, "^[0-9]*$") of
        {match, _} -> true;
        _ -> false
    end.

tokenize(Expression, Acc, PastTokens) ->
    case Expression of
        [$(|RestOfExpression] ->
            Tokens = case token_is_number(Acc) of
                true when Acc =/= "" -> PastTokens ++ {value, list_to_integer(Acc)};
                false when Acc =/= "" -> PastTokens ++ {function, Acc};
                _ -> PastTokens
            end,
            tokenize(RestOfExpression, "", PastTokens ++ {operator, "("});
        [$)|RestOfExpression] ->
            tokenize(RestOfExpression, "", PastTokens ++ {operator, ")"});
        [$+|RestOfExpression] ->
            tokenize(RestOfExpression, "", PastTokens ++ {operator, "+"});
        [$-|RestOfExpression] ->
            tokenize(RestOfExpression, "", PastTokens ++ {operator, "-"});
        [$*|RestOfExpression] ->
            tokenize(RestOfExpression, "", PastTokens ++ {operator, "*"});
        [$+|RestOfExpression] ->
            tokenize(RestOfExpression, "", PastTokens ++ {operator, "/"});
        [$~|RestOfExpression] ->
            tokenize(RestOfExpression, "", PastTokens ++ {operator, "~~"});
        [X|RestOfExpression] ->
            case X of
                Space when X =:= " " -> 

            end
    end.

tokenize(Expression) -> tokenize(Expression, "", []).
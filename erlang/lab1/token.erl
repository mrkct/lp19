-module(token).
-export([is_number/1, is_operator/1, is_parenthesis/1, precedence/1, operand_to_atom/1]).


is_number(S) -> 
    case re:run(S, "^[0-9]*$") of
        {match, _} -> true;
        _ -> false
    end.

is_operator(S) -> 
    case S of
        "+" -> true;
        "-" -> true;
        "*" -> true;
        "/" -> true;
        _   -> false
    end.

is_parenthesis(S) ->
    case S of
        "(" -> true;
        ")" -> true;
        _   -> false
    end.

precedence(Tok) ->
    case Tok of
        "+" -> 2;
        "-" -> 2;
        "*" -> 3;
        "/" -> 3
    end.

operand_to_atom(Op) ->
    case Op of
        "+" -> add;
        "-" -> subtract;
        "*" -> multiply;
        "/" -> divide
    end.
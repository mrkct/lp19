-module(parser).
-compile(export_all).


%% This exercise asks you to build a collection of functions that manipulate 
%% arithmetical expressions. Start with an expression such as the following: 
%%      ((2+3)-4), 4 and ~((2*3)+(3*4)) 
%% which is fully bracketed and where you use a tilde (~) for unary minus.
%%
%% First, write a parser for these, turning them into Erlang representations, 
%% such as the following: 
%%      {minus, {plus, {num, 2}, {num,3}}, {num, 4}} 
%% which represents ((2+3)-4). We call these exp s. Now, write an evaluator, 
%% which takes an exp and returns its value.

%% You can also extend the collection of expressions to add conditionals: 
%% if ((2+3)-4) then 4 else ~((2*3)+(3*4)) 
%% where the value returned is the “then” value if the “if” expression 
%% evaluates to 0, and it is the “else” value otherwise.

create_expr_stack(Operator, [A, B|Tail]) -> 
    case Operator of
        "(" -> [A, B|Tail];
        "+" -> [{add, B, A}|Tail];
        "-" -> [{subtract, B, A}|Tail];
        "*" -> [{multiply, B, A}|Tail];
        "/" -> [{divide, B, A}|Tail];
        "~~" -> [{negate, A}, B|Tail]
    end.

pop_until(OperatorStack, ExpressionStack, Condition) ->
    case OperatorStack of
        [] -> {OperatorStack, ExpressionStack};
        [{operator, Op}|Tail] -> 
            case Condition(Op) of
                true -> pop_until(Tail, create_expr_stack(Op, ExpressionStack), Condition);
                false -> {OperatorStack, ExpressionStack}
            end
    end.

parse(Expression) -> shunting_yard(tokenize(Expression), [], []).

shunting_yard(Tokens, OperatorStack, ExpressionStack) -> 
    case Tokens of
        [] -> 
            {_, [Result|_]} = pop_until(OperatorStack, ExpressionStack, fun (_X) -> true end),
            Result;
        [H|Rest]    -> handle_token(H, Rest, OperatorStack, ExpressionStack)
    end.

handle_token(Token, Rest, OperatorStack, ExpressionStack) ->
    case Token of
        {value, X}      -> shunting_yard(Rest, OperatorStack, [{num, X}|ExpressionStack]);
        {operator, "("} -> shunting_yard(Rest, [{operator, "("}|OperatorStack], ExpressionStack);
        {operator, ")"} -> 
            {[_SkipOpenParenthesis|NewOperatorStack], NewExpressionStack} = pop_until(
                OperatorStack,
                ExpressionStack,
                fun (Op) -> Op =/= "(" end
            ),
            shunting_yard(Rest, NewOperatorStack, NewExpressionStack);
        {operator, Op}  -> 
            {NewOperatorStack, NewExpressionStack} = handle_operator(Op, OperatorStack, ExpressionStack),
            shunting_yard(Rest, NewOperatorStack, NewExpressionStack)
    end.

handle_operator(Operator, OperatorStack, ExpressionStack) ->
    {NewOperatorStack, NewExpressionStack} = pop_until(
        OperatorStack, 
        ExpressionStack, 
        fun(Op) -> 
            (
                token:precedence(Operator) < token:precedence(Op) 
                orelse
                (token:precedence(Operator) =:= token:precedence(Op) andalso token:left_assoc(Op))
            )
            andalso
            Op =/= "("
        end
    ),
    {[{operator, Operator}|NewOperatorStack], NewExpressionStack}.

%% SampleE1 = [{value, 1}, {operator, "+"}, {value, 2}].
%% SampleE2 = [{value, 1}, {operator, "+"}, {value, 2}, {operator, "*"}, {value, 5}].

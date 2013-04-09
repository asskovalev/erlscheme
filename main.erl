-module(main).
-export([run/1, eval/2, ast/1]).

eval(Expr) ->
    Env = env:init(),
    eval(Env, Expr).

eval(_Env, It = {symbol,"#t"}) -> It;
eval(_Env, It = {symbol,"#f"}) -> It;
eval(_Env, It = {number,_}) -> It;
eval(_Env, It = {string,_}) -> It;
eval(Env, {symbol, S}) -> env:lookup(S, Env);
eval(Env, {list, [Hd]}) -> eval(Env, Hd);
eval(Env, {list, [Hd|Tail]}) ->
    case eval(Env, Hd) of
        {function, F} -> appl(Env, F, Tail);
        {special, F} -> F(Env, Tail);
        {definition, NewEnv} -> eval(NewEnv, {list, Tail});
        _ -> eval(Env, {list, Tail})
    end.

appl(Env, F, Args) ->
    F([eval(Env, Arg) || Arg <- Args]).


ast(Input) ->
    parser:run(Input).

run(Input) ->
    Expr = parser:run(Input),
    eval(Expr).


       
    

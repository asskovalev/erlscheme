-module(core).
-compile(export_all).

-define(num(N), {number, N}).
-define(str(S), {string, S}).
-define(lst(List), {list, List}).
-define(sym(Name), {symbol, Name}).

-define(true,  ?sym("#t")).
-define(false, ?sym("#f")).
-define(unquote(Body), ?lst([?sym("unquote"), Body])).

-define(binding(Name, Expr), ?lst([?sym(Name), Expr])).
-define(signature(Name, Params), ?lst([?sym(Name)|Params])).

-define(eval(Env, Expr), main:eval(Env, Expr)).
-define(bind(Name, Env, Expr), {Name, ?eval(Env, Expr)}).


fn(Fn) -> {function, Fn}.
special(Fn) -> {special, Fn}.
definition(NewEnv) -> {definition, NewEnv}.
     

n_add([?num(Acc)|Rest]) -> ?num(lists:foldl(fun (?num(N), A) -> A + N end, Acc, Rest)).
n_sub([?num(Acc)|Rest]) -> ?num(lists:foldl(fun (?num(N), A) -> A - N end, Acc, Rest)).
n_mul([?num(Acc)|Rest]) -> ?num(lists:foldl(fun (?num(N), A) -> A * N end, Acc, Rest)).
n_div([?num(Acc)|Rest]) -> ?num(lists:foldl(fun (?num(N), A) -> A / N end, Acc, Rest)).

eq([?num(X)|Rest]) ->
    Result = lists:all(fun (?num(N)) -> X =:= N end, Rest),
    case Result of 
        true -> ?true;
        false -> ?false 
    end.



s_if(Env, [Cond, True, False]) ->
    case ?eval(Env, Cond) of
        ?false    -> ?eval(Env, False);
        ?lst([])  -> ?eval(Env, False);
        ?str("")  -> ?eval(Env, False);
        ?num(0)   -> ?eval(Env, False);
        ?num(0.0) -> ?eval(Env, False);
        _ -> ?eval(Env, True)
    end.

s_let(Env, [?lst(Bindings), Body]) ->
    Map = fun (?binding(Name, Expr)) ->
                  ?bind(Name, Env, Expr)
          end,
    Evaled = lists:map(Map, Bindings),
    NewEnv = env:extend(Env, Evaled),
    ?eval(NewEnv, Body).

s_let_star(Env, [?lst(Bindings), Body]) ->
    Fold = fun (?binding(Name, Expr), Acc) -> 
                   env:extend(Acc, ?bind(Name, Acc, Expr))
           end,
    NewEnv = lists:foldl(Fold, Env, Bindings),
    ?eval(NewEnv, Body).

s_lambda(_Env, [?lst(Params), Body]) ->
    make_lambda(Params, Body).

s_quote(Env, [?lst(Body)]) ->
    Iter = fun (?unquote(Unq)) -> ?eval(Env, Unq);
               (Item) -> Item
           end,
    ?lst(lists:map(Iter, Body));

s_quote(_Env, [Body]) -> Body.
                   

s_define(Env, [?signature(Name, Params), Body]) ->
    Closure = make_lambda(Params, Body),
    NewEnv = env:extend(Env, {Name, Closure}),
    definition(NewEnv).

make_lambda(Params, Body) ->
    Closure = fun(Env, Args) ->
                      Bindings = lists:zipwith(
                                   fun (?sym(Name), Expr) -> 
                                           ?bind(Name, Env, Expr) 
                                   end,
                                   Params, 
                                   Args),
                      NewEnv = env:extend(Env, Bindings),
                      ?eval(NewEnv, Body)
              end,
    special(Closure).

-module(main).
-export([process/1, ast/1, eval/1]).

fn(Fn) -> {function, Fn}.
special(Fn) -> {special, Fn}.
                      

plus([{number, Acc}|Rest]) ->
    Result = lists:foldl(fun ({number, N}, A) -> A + N end, Acc, Rest),
    {number, Result}.

minus([{number, Acc}|Rest]) ->
    Result = lists:foldl(fun ({number, N}, A) -> A - N end, Acc, Rest),
    {number, Result}.

s_if(Env, [Cond, True, False]) ->
    case eval(Env, Cond) of
        {list, []} -> eval(Env, False);
        {string, ""} -> eval(Env, False);
        {number, 0} -> eval(Env, False);
        {number, 0.0} -> eval(Env, False);
        _ -> eval(Env, True)
    end.

s_let(Env, [{list, Bindings}, Body]) ->
    Evaled = [{Name, eval(Env, Expr)} || {list, [{symbol, Name}, Expr]} <- Bindings],
    NewEnv = extend_env(Env, Evaled),
    eval(NewEnv, Body).

s_let_star(Env, [{list, Bindings}, Body]) ->
    NewEnv = lists:foldl(fun ({list, [{symbol, Name}, Expr]}, EnvAcc) ->
                                 Evaled = eval(EnvAcc, Expr),
                                 NewEnvAcc = extend_env(EnvAcc, {Name, Evaled}),
                                 NewEnvAcc
                         end, 
                         Env,
                         Bindings),
    eval(NewEnv, Body).

s_let_rec(Env, [{list, Bindings}, Body]) ->
    todo.

s_lambda(_Env, [{list, Parameters}, Body]) ->
    Closure = fun(Env, Args) ->
                      Bindings = lists:zipwith(
                                   fun ({symbol, Parameter}, Value) -> {Parameter, Value} end,
                                   Parameters, 
                                   Args),
                      NewEnv = extend_env(Env, Bindings),
                      eval(NewEnv, Body)
              end,
    special(Closure).

env() ->
    [[{"+", fn(fun plus/1)},
      {"-", fn(fun minus/1)},
      {"if", special(fun s_if/2)},
      {"let", special(fun s_let/2)},
      {"let*", special(fun s_let_star/2)},
      {"lambda", special(fun s_lambda/2)}]].

extend_env(Env, Binding={_Name, _Value}) ->
    [[Binding]|Env];
extend_env(Env, Bindings) ->
    [Bindings|Env].

lookup(What, []) ->
    {error, not_found, What};

lookup(What, [Env|Rest]) ->
    case proplists:is_defined(What, Env) of
        true -> proplists:get_value(What, Env);
        false -> lookup(What, Rest)
    end.
                 

eval(Expr) ->
    Env = env(),
    eval(Env, Expr).

eval(_Env, It = {number, _}) -> It;
eval(_Env, It = {string, _}) -> It;
eval(Env, {symbol, S}) -> lookup(S, Env);
eval(Env, {list, [Hd|Tail]}) ->
    case eval(Env, Hd) of
        {function, F} -> appl(Env, F, Tail);
        {special, F} -> F(Env, Tail)
    end.

appl(Env, F, Args) ->
    F([eval(Env, Arg) || Arg <- Args]).

ast({num, Num}) -> {number, Num};
ast({str, Str}) -> {string, Str};
ast({sym, Sym}) -> {symbol, Sym};
ast(List) -> {list, [ast(Item) || Item <- List]}.




process(Input) ->
    {[Data], []} = parse(Input),
    Expr = ast(Data),
    eval(Expr).


read_atom(Str) ->
    {A, Rest} = read_atom([], Str),
    Atom = categorize(lists:reverse(A)),
    {Atom, Rest}.

read_atom(Acc, []) -> {Acc, []};
read_atom(Acc, [($  )|Rest]) -> {Acc, Rest};
read_atom(Acc, Rest=[($( )|_]) -> {Acc, Rest};
read_atom(Acc, Rest=[($) )|_]) -> {Acc, Rest};
read_atom(Acc, [Char|Rest]) -> read_atom([Char|Acc], Rest).

read_string(Str) ->
    {A, Rest} = read_string([$"], Str),
    Atom = categorize(lists:reverse(A)),
    {Atom, Rest}.

read_string(Acc, []) -> {Acc, []};
read_string(Acc, [$\\,$"|Rest]) -> {[$"|Acc], Rest};
read_string(Acc, [$"|Rest]) -> {Acc, Rest};
read_string(Acc, [Char|Rest]) -> read_string([Char|Acc], Rest).


parse(Input) -> 
    {R, Rest} = parse([], Input),
    {lists:reverse(R), Rest}.

parse(Acc, []) -> {Acc, []};
parse(Acc, [($ )|Rest]) -> parse(Acc, Rest);
parse(Acc, [($))|Rest]) -> {Acc, Rest};
parse(Acc, [($()|Rest]) -> 
    {Child, NewRest} = parse(Rest),
    parse([Child|Acc], NewRest);
parse(Acc, [($")|Rest]) -> 
    {Str, NewRest} = read_string(Rest),
    parse([Str|Acc], NewRest);
parse(Acc, Rest) -> 
    {Atom, NewRest} = read_atom(Rest),
    parse([Atom|Acc], NewRest).


categorize([$"|Atom]) -> {str, Atom};
categorize(Atom) -> 
    case string:to_float(Atom) of
        {Float, []} -> {num, Float};
        _ -> case string:to_integer(Atom) of
                 {Int, []} -> {num, Int};
                 _ -> {sym, Atom}
             end
    end.
            
       
    

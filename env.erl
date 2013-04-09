-module(env).
-export([init/0, extend/2, lookup/2]).

empty() -> [].

init() ->
    extend(empty(), 
           [{"+",      core:fn(fun core:n_add/1)},
            {"-",      core:fn(fun core:n_sub/1)},
            {"*",      core:fn(fun core:n_mul/1)},
            {"/",      core:fn(fun core:n_div/1)},
            {"eq",     core:fn(fun core:eq/1)},
            {"define", core:special(fun core:s_define/2)},
            {"if",     core:special(fun core:s_if/2)},
            {"let",    core:special(fun core:s_let/2)},
            {"let*",   core:special(fun core:s_let_star/2)},
            {"lambda", core:special(fun core:s_lambda/2)}]).

extend(Env, Binding={_,_}) -> 
    extend(Env, [Binding]);
extend(Env, Bindings) -> 
    [dict:from_list(Bindings)|Env].

lookup(What, []) ->
    {error, not_found, What};

lookup(What, [Frame|Rest]) ->
    case lookup_frame(What, Frame) of
        {ok, Value} -> Value;
        {error, not_found} -> lookup(What, Rest)
    end.

lookup_frame(What, Frame) ->
    case dict:is_key(What, Frame) of
        true -> {ok, dict:fetch(What, Frame)};
        false -> {error, not_found}
    end.


-module(parser).
-export([run/1, parse/1]).

run(Input) ->
    {Data, []} = parse(Input),
    ast(Data).
     
ast({num, Num}) -> {number, Num};
ast({str, Str}) -> {string, Str};
ast({sym, Sym}) -> {symbol, Sym};
ast({q, Item})  -> {list, [{symbol, "quote"}, ast(Item)]};
ast({uq, Item}) -> {list, [{symbol, "unquote"}, ast(Item)]};
ast({lst, List}) -> {list, [ast(Item) || Item <- List]};
ast([Item]) -> ast(Item);
ast(List = [_Item|_Rest]) -> {list, [ast(Item) || Item <- List]}.

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

parse(Acc, [($'),($()|Rest]) -> 
    {Child, NewRest} = parse(Rest),
    parse([{q, Child}|Acc], NewRest);

parse(Acc, [($,),($()|Rest]) -> 
    {Child, NewRest} = parse(Rest),
    parse([{uq, Child}|Acc], NewRest);


parse(Acc, [($')|Rest]) -> 
    {Child, NewRest} = read_atom(Rest),
    parse([{q, Child}|Acc], NewRest);

parse(Acc, [($,)|Rest]) -> 
    {Child, NewRest} = read_atom(Rest),
    parse([{uq, Child}|Acc], NewRest);


parse(Acc, [($()|Rest]) -> 
    {Child, NewRest} = parse(Rest),
    parse([{lst, Child}|Acc], NewRest);

parse(Acc, [($")|Rest]) -> 
    {Str, NewRest} = read_string(Rest),
    parse([{str, Str}|Acc], NewRest);

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
            

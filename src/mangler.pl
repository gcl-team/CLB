:- module(mangler, [mangle/3]).

% The "Private" Reserved Word List
reserved(W) :- member(W, ['TO', 'OR', 'IF', 'ON', 'GO', 'AS', 'ST', 'FN']).

% The "Public" Predicate
mangle(LongName, Used, Safe) :-
    string_upper(LongName, Upper),
    sub_atom(Upper, 0, 2, _, Try),
    resolve_collision(Try, Used, Safe).

resolve_collision(Try, Used, Safe) :-
    (reserved(Try) ; member(Try, Used)), !,
    sub_atom(Try, 0, 1, _, First),
    sub_atom(Try, 1, 1, _, Second),
    char_code(Second, Code),
    next_char_code(Code, NextCode),
    char_code(NextChar, NextCode),
    atom_concat(First, NextChar, NextTry),
    resolve_collision(NextTry, Used, Safe).
resolve_collision(Safe, _, Safe).

next_char_code(90, 48) :- !. % 'Z' -> '0'
next_char_code(57, 65) :- !. % '9' -> 'A' (should not happen in this flow but for safety)
next_char_code(C, N) :- N is C + 1.
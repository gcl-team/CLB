:- module(mangler, [mangle/3]).

% The "Private" Reserved Word List
reserved(W) :- member(W, ['TO', 'OR', 'IF', 'ON', 'GO', 'AS', 'ST', 'FN']).

% The "Public" Predicate
mangle(LongName, Used, Safe) :-
    upcase_atom(LongName, Upper),
    atom_chars(Upper, Chars),
    include(is_alnum, Chars, CleanChars),
    atom_chars(Clean, CleanChars),
    atom_length(Clean, Len),
    ( Len < 2 -> Try = Clean ; sub_atom(Clean, 0, 2, _, Try) ),
    resolve_collision(Try, Used, Safe).

is_alnum(C) :- char_type(C, alnum).

resolve_collision(Try, Used, Safe) :-
    resolve_collision(Try, Used, [], Safe).

resolve_collision(Try, Used, Seen, Safe) :-
    (reserved(Try) ; member(Try, Used)), !,
    ( member(Try, Seen) ->
        format('FATAL ERROR: Variable name space exhausted for prefix "~w". Too many collisions.~n', [Try]),
        fail
    ;   true
    ),
    atom_length(Try, Len),
    ( Len =:= 1 ->
        % If it is only 1 char, add a 0 to make it 2 chars
        atom_concat(Try, '0', NextTry),
        resolve_collision(NextTry, Used, [Try|Seen], Safe)
    ;   % If 'AB' is taken, try 'AC', 'AD'...
        sub_atom(Try, 0, 1, _, First),
        sub_atom(Try, 1, 1, _, Second),
        char_code(Second, Code),
        ( next_char_code(Code, NextCode) ->
            char_code(NextChar, NextCode),
            atom_concat(First, NextChar, NextTry),
            resolve_collision(NextTry, Used, [Try|Seen], Safe)
        ;   format('FATAL ERROR: Variable name space exhausted for prefix "~w".~n', [First]),
            fail
        )
    ).
resolve_collision(Safe, _, _, Safe).

next_char_code(90, 48) :- !. % 'Z' -> '0'
next_char_code(57, _) :- !, fail. % '9' -> STOP (No more characters)
next_char_code(C, N) :- N is C + 1.
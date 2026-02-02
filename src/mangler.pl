:- module(mangler, [mangle/4]).

% The "Private" Reserved Word List
% These are words that cannot be used as variable names because BASIC V2 would misinterpret them.
reserved(W) :- member(W, ['TO', 'OR', 'IF', 'ON', 'GO', 'AS', 'ST', 'FN']).

% The "Public" Predicate
% LongName: The C-like variable name
% Used: List of already used *full* BASIC names (e.g. ['SC%', 'NA$'])
% Suffix: '%' for int/bool, '$' for string
% Safe: The final safe BASIC name (e.g. 'SD$')
mangle(LongName, Used, Suffix, FinalSafe) :-
    upcase_atom(LongName, Upper),
    atom_chars(Upper, Chars),
    include(is_alnum, Chars, CleanChars),
    atom_chars(Clean, CleanChars),
    atom_length(Clean, Len),
    ( Len < 1 -> Try = 'V' ; ( Len < 2 -> Try = Clean ; sub_atom(Clean, 0, 2, _, Try) ) ),
    resolve_collision(Try, Used, Suffix, [], FinalSafe).

is_alnum(C) :- char_type(C, alnum).

resolve_collision(Try, Used, Suffix, Seen, FinalSafe) :-
    atom_concat(Try, Suffix, Candidate),
    ( (reserved(Try) ; member(Candidate, Used)) ->
        ( member(Try, Seen) ->
            format('FATAL ERROR: Variable name space exhausted for prefix "~w". Too many collisions.~n', [Try]),
            fail
        ;   true
        ),
        atom_length(Try, Len),
        ( Len =:= 1 ->
            % If it is only 1 char, add a 0 to make it 2 chars
            atom_concat(Try, '0', NextTry),
            resolve_collision(NextTry, Used, Suffix, [Try|Seen], FinalSafe)
        ;   % If 'AB' is taken, try 'AC', 'AD'...
            sub_atom(Try, 0, 1, _, First),
            sub_atom(Try, 1, 1, _, Second),
            char_code(Second, Code),
            ( next_char_code(Code, NextCode) ->
                char_code(NextChar, NextCode),
                atom_concat(First, NextChar, NextTry),
                resolve_collision(NextTry, Used, Suffix, [Try|Seen], FinalSafe)
            ;   format('FATAL ERROR: Variable name space exhausted for prefix "~w".~n', [First]),
                fail
            )
        )
    ;   FinalSafe = Candidate
    ).

next_char_code(90, 48) :- !. % 'Z' -> '0'
next_char_code(57, _) :- !, fail. % '9' -> STOP (No more characters)
next_char_code(C, N) :- N is C + 1.
:- module(lexer_tests, []).
:- use_module('../src/lexer').

:- begin_tests(lexer).

test(tokenize_assignment) :-
    tokenize("int x = 10;", [int, x, '=', 10, ';']).

test(tokenize_comparison) :-
    tokenize("int x = 1 == 1;", [int, x, '=', 1, '==', 1, ';']),
    tokenize("int y = 1 != 0;", [int, y, '=', 1, '!=', 0, ';']).

test(tokenize_string) :-
    tokenize("print(\"HI\");", [print, '(', '"HI"', ')', ';']).

:- end_tests(lexer).

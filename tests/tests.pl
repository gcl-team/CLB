:- module(tests, [run_all_tests/0]).

:- use_module(src/compiler).
:- use_module(src/lexer).
:- use_module(src/mangler).

:- begin_tests(mangler).

test(simple_mangle) :-
    mangle(health, [], 'HE').

test(collision_mangle) :-
    mangle(heart, ['HE'], 'HF').

test(reserved_mangle) :-
    mangle(to, [], 'TP'). % TO is reserved, next is TP

:- end_tests(mangler).

:- begin_tests(lexer).

test(tokenize_assignment) :-
    tokenize("int x = 10;", [int, x, =, 10, ;]).

test(tokenize_string) :-
    tokenize("print(\"HI\");", [print, '(', "\"HI\"", ')', ;]).

:- end_tests(lexer).

run_all_tests :-
    run_tests.

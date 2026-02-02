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
    tokenize("int x = 10;", [int, x, '=', 10, ';']).

test(tokenize_comparison) :-
    tokenize("int x = 1 == 1;", [int, x, '=', 1, '==', 1, ';']),
    tokenize("int y = 1 != 0;", [int, y, '=', 1, '!=', 0, ';']).

test(tokenize_string) :-
    tokenize("print(\"HI\");", [print, '(', '"HI"', ')', ';']).

:- end_tests(lexer).

:- begin_tests(compiler).

test(compile_comparison) :-
    % use_module(src/compiler) is already done at top level
    lexer:tokenize('int x = 1 == 1;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 X% = 1=1'.

:- end_tests(compiler).

run_all_tests :-
    run_tests.

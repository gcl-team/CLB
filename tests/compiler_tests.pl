:- module(compiler_tests, []).
:- use_module('../src/compiler').
:- use_module('../src/lexer').

:- begin_tests(compiler).

test(compile_assignment) :-
    lexer:tokenize('int x = 10;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 X% = 10'.

test(compile_comparison) :-
    lexer:tokenize('int x = 1 == 1;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 X% = 1=1'.

test(compile_not_equal) :-
    lexer:tokenize('int y = 5 != 3;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 Y% = 5<>3'.

:- end_tests(compiler).

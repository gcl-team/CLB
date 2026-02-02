:- module(expression_tests, []).
:- use_module('../../src/compiler/expressions').
:- use_module('../../src/lexer').

:- begin_tests(expressions).

test(simple_math) :-
    lexer:tokenize('5 + 3', Tokens),
    phrase(compiler_expressions:expression(Code, []), Tokens),
    Code = '5+3'.

test(more_math) :-
    lexer:tokenize('10 * 2', Tokens),
    phrase(compiler_expressions:expression(Code, []), Tokens),
    Code = '10*2'.

test(boolean_logic) :-
    lexer:tokenize('true && false', Tokens),
    phrase(compiler_expressions:expression(Code, []), Tokens),
    % true maps to -1, false to 0
    Code = '-1 AND 0'.

test(comparison) :-
    lexer:tokenize('score >= 100', Tokens),
    phrase(compiler_expressions:expression(Code, [score-'SC%']), Tokens),
    Code = 'SC%>=100'.

test(modulo) :-
    lexer:tokenize('10 % 3', Tokens),
    phrase(compiler_expressions:expression(Code, []), Tokens),
    Code = '10-(INT(10/3)*3)'.

:- end_tests(expressions).

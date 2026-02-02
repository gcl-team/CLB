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

test(compile_bool) :-
    lexer:tokenize('bool active = true;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 AC% = -1'.

test(compile_bool_false) :-
    lexer:tokenize('bool running = false;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 RU% = 0'.

test(compile_logical_and) :-
    lexer:tokenize('bool res = true && false;', Tokens),
    phrase(compiler:program(10, [], _, [Line]), Tokens),
    Line = '10 RE% = -1 AND 0'.

test(compile_logical_or) :-
    lexer:tokenize('bool res = active || false;', Tokens),
    phrase(compiler:program(10, [active-'AC%'], _, [Line]), Tokens),
    Line = '10 RE% = AC% OR 0'.

test(compile_logical_not) :-
    lexer:tokenize('bool res = !active;', Tokens),
    phrase(compiler:program(10, [active-'AC%'], _, [Line]), Tokens),
    Line = '10 RE% = NOT(AC%)'.

test(compile_if) :-
    lexer:tokenize('if (1 == 1) { print("HI"); }', Tokens),
    phrase(compiler:program(10, [], _, Lines), Tokens),
    Lines = ['10 IF NOT(1=1) GOTO 30', '20 PRINT "HI"'].

test(compile_if_nested_or_sequential) :-
    lexer:tokenize('if (1 == 1) { print("HI"); } int x = 5;', Tokens),
    phrase(compiler:program(10, [], _, Lines), Tokens),
    Lines = ['10 IF NOT(1=1) GOTO 30', '20 PRINT "HI"', '30 X% = 5'].

test(compile_while) :-
    lexer:tokenize('int x = 0; while (x < 10) { x = x + 1; }', Tokens),
    phrase(compiler:program(10, [], _, Lines), Tokens),
    Lines = [
        '10 X% = 0',
        '20 IF NOT(X%<10) GOTO 50',
        '30 X% = X%+1',
        '40 GOTO 20'
    ].

test(compile_if_else) :-
    lexer:tokenize('if (1 == 1) { print("A"); } else { print("B"); }', Tokens),
    phrase(compiler:program(10, [], _, Lines), Tokens),
    Lines = [
        '10 IF NOT(1=1) GOTO 40',
        '20 PRINT "A"',
        '30 GOTO 50',
        '40 PRINT "B"'
    ].

:- end_tests(compiler).

:- module(tests, [run_all_tests/0]).

% Load all test modules
:- use_module(lexer_tests).
:- use_module(mangler_tests).
:- use_module(compiler_tests).
:- use_module(integration_tests).

run_all_tests :-
    run_tests.

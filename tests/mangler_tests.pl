:- module(mangler_tests, []).
:- use_module('../src/mangler').

:- begin_tests(mangler).

test(simple_mangle) :-
    mangle(health, [], 'HE').

test(collision_mangle) :-
    mangle(heart, ['HE'], 'HF').

test(reserved_mangle) :-
    mangle(to, [], 'TP'). % TO is reserved, next is TP

:- end_tests(mangler).

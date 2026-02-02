:- module(mangler_tests, []).
:- use_module('../src/mangler').

:- begin_tests(mangler).

test(simple_mangle) :-
    mangle(health, [], '%', 'HE%').

test(string_mangle) :-
    mangle(name, [], '$', 'NA$').

test(collision_mangle) :-
    mangle(heart, ['HE%'], '%', 'HF%').

test(cross_type_no_collision) :-
    mangle(score, ['SC$'], '%', 'SC%'). % SC$ and SC% do not collide in BASIC

test(reserved_mangle) :-
    mangle(to, [], '%', 'TP%'). % TO is reserved, next is TP

:- end_tests(mangler).

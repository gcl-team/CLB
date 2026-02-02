:- module(compiler_utils, [resolve_val/3, clb_to_basic_op/2]).

% Helper to resolve a value (either a literal or a variable)
resolve_val(true, _, '-1') :- !.
resolve_val(false, _, '0') :- !.
resolve_val(Val, State, Basic) :-
    memberchk(Val-Basic, State), !.
resolve_val(Val, _, Val). % Assume literal if not in symbol table

% Mapping from CLB to BASIC operators
clb_to_basic_op('+', '+').
clb_to_basic_op('-', '-').
clb_to_basic_op('*', '*').
clb_to_basic_op('/', '/').
clb_to_basic_op('==', '=').
clb_to_basic_op('!=', '<>').
clb_to_basic_op('<', '<').
clb_to_basic_op('>', '>').
clb_to_basic_op('<=', '<=').
clb_to_basic_op('>=', '>=').
clb_to_basic_op('&&', ' AND ').
clb_to_basic_op('||', ' OR ').

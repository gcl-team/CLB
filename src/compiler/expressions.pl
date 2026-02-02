:- module(compiler_expressions, [expression//2]).

:- use_module(utils).

% --- EXPRESSION PARSER ---
expression(Code, State) -->
    ['!'], expression(Sub, State), !,
    {
        atomic_list_concat(['NOT(', Sub, ')'], Code)
    }.
expression(Code, State) -->
    [A], ['%'], [B], !,
    {
        resolve_val(A, State, ABasic),
        resolve_val(B, State, BBasic),
        atomic_list_concat([ABasic, '-(INT(', ABasic, '/', BBasic, ')*', BBasic, ')'], Code)
    }.
expression(Code, State) -->
    [A], [Op], [B],
    {
        clb_to_basic_op(Op, BasicOp), !,
        resolve_val(A, State, ABasic),
        resolve_val(B, State, BBasic),
        atomic_list_concat([ABasic, BasicOp, BBasic], Code)
    }.
expression(Basic, State) -->
    [Val],
    { 
        resolve_val(Val, State, Basic) 
    }.

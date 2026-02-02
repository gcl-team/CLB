:- module(compiler, [compile_to_console/1, compile_file/2, compile_code/1]).

:- use_module(library(dcg/basics)).
:- use_module(mangler).
:- use_module(lexer).

% --- THE PARSER (DCG) ---
% program(CurrentLine, SymbolTableIn, SymbolTableOut, ListOfLines)
program(Line, _, _, _) -->
    { Line > 63999, !, format('FATAL ERROR: Line number ~w exceeds C64 limit of 63999.~n', [Line]), fail }.
program(_, State, State, []) --> [].
program(Line, StateIn, StateOut, FinalLines) -->
    statement(Line, NextLine, StateIn, StateNext, Code),
    { 
        (is_list(Code) -> Lines = Code ; Lines = [Code])
    },
    program(NextLine, StateNext, StateOut, Rest),
    { append(Lines, Rest, FinalLines) }.

% Rule: int Name = Expr;
statement(Line, NextLine, StateIn, StateOut, FinalCode) -->
    [Type], { member(Type, [int, bool]) }, [Name], ['='], expression(Expr, StateIn), [';'],
    { 
        NextLine is Line + 10,        (member(Name-_, StateIn) -> 
            format('FATAL ERROR: Variable "~w" is already declared.~n', [Name]), fail 
        ;   true
        ),
        % Extract already used short names (base names without the % type suffix)
        findall(Short, (member(_-Full, StateIn), atom_concat(Short, '%', Full)), Used),
        mangle(Name, Used, Short),
        atom_concat(Short, '%', BasicVar),
        % Update Symbol Table
        append(StateIn, [Name-BasicVar], StateOut),
        atomic_list_concat([Line, ' ', BasicVar, ' = ', Expr], FinalCode)
    }.

% Rule: Name = Expr;
statement(Line, NextLine, State, State, FinalCode) -->
    [Name], ['='], expression(Expr, State), [;],
    {
        NextLine is Line + 10,
        resolve_val(Name, State, BasicVar),
        atomic_list_concat([Line, ' ', BasicVar, ' = ', Expr], FinalCode)
    }.

% Rule: if (Condition) { Statements }
statement(Line, NextLine, StateIn, StateOut, [IfLine|BlockLines]) -->
    [if], ['('], expression(Cond, StateIn), [')'], ['{'],
    { BodyStart is Line + 10 },
    program(BodyStart, StateIn, StateOut, BlockLines),
    ['}'],
    {
        % Find the last line number in the block to know where to jump to
        ( last(BlockLines, LastLineCode) ->
            atomic_list_concat([LastNumAtom|_], ' ', LastLineCode),
            atom_number(LastNumAtom, LastNum),
            JumpLine is LastNum + 10
        ;   JumpLine is Line + 10 % Empty block
        ),
        NextLine is JumpLine,
        atomic_list_concat([Line, ' IF NOT(', Cond, ') GOTO ', JumpLine], IfLine)
    }.

% Rule: while (Condition) { Statements }
statement(Line, NextLine, StateIn, StateOut, [WhileLine|FinalBodyLines]) -->
    [while], ['('], expression(Cond, StateIn), [')'], ['{'],
    { BodyStart is Line + 10 },
    program(BodyStart, StateIn, StateOut, BlockLines),
    ['}'],
    {
        % BackJump follows the last line of the block
        ( last(BlockLines, LastLineCode) ->
            atomic_list_concat([LastNumAtom|_], ' ', LastLineCode),
            atom_number(LastNumAtom, LastNum),
            BackJumpLine is LastNum + 10
        ;   BackJumpLine is Line + 10 % Empty loop
        ),
        ExitLine is BackJumpLine + 10,
        NextLine is ExitLine,
        atomic_list_concat([Line, ' IF NOT(', Cond, ') GOTO ', ExitLine], WhileLine),
        atomic_list_concat([BackJumpLine, ' GOTO ', Line], BackJumpCode),
        append(BlockLines, [BackJumpCode], FinalBodyLines)
    }.

% Rule: poke(address, value);
statement(Line, NextLine, State, State, FinalCode) -->
    [poke], ['('], [Addr], [','], [Val], [')'], [';'],
    {
        NextLine is Line + 10,
        resolve_val(Addr, State, AddrBasic),
        resolve_val(Val, State, ValBasic),
        atomic_list_concat([Line, ' POKE ', AddrBasic, ',', ValBasic], FinalCode)
    }.

% Rule: print("string"); or print(variable);
statement(Line, NextLine, State, State, FinalCode) -->
    [print], ['('], [Content], [')'], [';'],
    { 
        NextLine is Line + 10,
        resolve_val(Content, State, BasicContent),
        atomic_list_concat([Line, ' PRINT ', BasicContent], FinalCode) 
    }.

% Rule: clear();
statement(Line, NextLine, State, State, FinalCode) -->
    [clear], ['('], [')'], [';'],
    { 
        NextLine is Line + 10,
        atomic_list_concat([Line, ' PRINT CHR$(147)'], FinalCode) 
    }.

% Helper to resolve a value (either a literal or a variable)
resolve_val(true, _, '-1') :- !.
resolve_val(false, _, '0') :- !.
resolve_val(Val, State, Basic) :-
    member(Val-Basic, State), !.
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

% --- THE COMPILER ENTRY POINT ---

% Option 1: Compile a raw string of CLB code to Console
compile_code(Source) :-
    tokenize(Source, Tokens),
    ( phrase(program(10, [], _, Lines), Tokens) ->
        atomic_list_concat(Lines, '\n', Final),
        format("~w~n", [Final])
    ;   format("ERROR: Parsing failed.~n")
    ).

% Option 2: Compile a file to Console
compile_to_console(Path) :-
    read_file_to_string(Path, Source, []),
    tokenize(Source, Tokens),
    ( phrase(program(10, [], _, Lines), Tokens) ->
        atomic_list_concat(Lines, '\n', Final),
        format("--- BASIC OUTPUT ---~n~w~n--------------------~n", [Final])
    ;   format("ERROR: Parsing failed in ~w~n", [Path])
    ).

% Option 3: Compile to File
compile_file(InPath, OutPath) :-
    read_file_to_string(InPath, Source, []),
    tokenize(Source, Tokens),
    ( phrase(program(10, [], _, Lines), Tokens) ->
        atomic_list_concat(Lines, '\n', Final),
        setup_call_cleanup(
            open(OutPath, write, Out),
            format(Out, "~w~n", [Final]),
            close(Out)
        ),
        format("Success: Compiled ~w to ~w~n", [InPath, OutPath])
    ;   format("ERROR: Parsing failed in ~w~n", [InPath])
    ).
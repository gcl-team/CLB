:- module(compiler, [compile_to_console/1, compile_file/2, compile_code/1]).

:- use_module(library(dcg/basics)).
:- use_module(mangler).
:- use_module(lexer).

% --- THE PARSER (DCG) ---
% program(CurrentLine, SymbolTableIn, SymbolTableOut)
program(Line, _, _, _) -->
    { Line > 63999, !, format('FATAL ERROR: Line number ~w exceeds C64 limit of 63999.~n', [Line]), fail }.
program(_, State, State, []) --> [].
program(Line, StateIn, StateOut, [Code|Rest]) -->
    statement(Line, StateIn, StateNext, Code),
    { NextLine is Line + 10 },
    program(NextLine, StateNext, StateOut, Rest).

% Rule: int Name = Value;
statement(Line, StateIn, StateOut, FinalCode) -->
    [int], [Name], [=], [Value], [;],
    { 
        % Extract already used short names (first 2 chars)
        findall(Short, (member(_-Full, StateIn), sub_atom(Full, 0, 2, _, Short)), Used),
        mangle(Name, Used, Short),
        atom_concat(Short, '%', BasicVar),
        % Update Symbol Table
        append(StateIn, [Name-BasicVar], StateOut),
        atomic_list_concat([Line, ' ', BasicVar, ' = ', Value], FinalCode)
    }.

% Rule: poke(address, value);
statement(Line, State, State, FinalCode) -->
    [poke, '(', Addr, ',', Val, ')', ;],
    {
        resolve_val(Addr, State, AddrBasic),
        resolve_val(Val, State, ValBasic),
        atomic_list_concat([Line, ' POKE ', AddrBasic, ',', ValBasic], FinalCode)
    }.

% Rule: print("string"); or print(variable);
statement(Line, State, State, FinalCode) -->
    [print, '(', Content, ')', ;],
    { 
        resolve_val(Content, State, BasicContent),
        atomic_list_concat([Line, ' PRINT ', BasicContent], FinalCode) 
    }.

% Rule: clear();
statement(Line, State, State, FinalCode) -->
    [clear, '(', ')', ;],
    { atomic_list_concat([Line, ' PRINT CHR$(147)'], FinalCode) }.

% Helper to resolve a value (either a literal or a variable)
resolve_val(Val, State, Basic) :-
    member(Val-Basic, State), !.
resolve_val(Val, _, Val). % Assume literal if not in symbol table

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
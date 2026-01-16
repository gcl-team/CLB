:- module(compiler, [compile/1, compile_file/1]).

:- use_module(library(dcg/basics)).
:- use_module(mangler).
:- use_module(lexer).

% --- THE PARSER (DCG) ---
% program(CurrentLine, SymbolTableIn, SymbolTableOut)
program(Line, _, _) -->
    { Line > 63999, !, format('FATAL ERROR: Line number ~w exceeds C64 limit of 63999.~n', [Line]), fail }.
program(_, State, State) --> [].
program(Line, StateIn, StateOut) -->
    statement(Line, StateIn, StateNext, Code),
    { format('~w~n', [Code]), NextLine is Line + 10 },
    program(NextLine, StateNext, StateOut).

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
compile(Source) :-
    tokenize(Source, Tokens),
    phrase(program(10, [], _), Tokens).

compile_file(Path) :-
    read_file_to_string(Path, Source, []),
    compile(Source).
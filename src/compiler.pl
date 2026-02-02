:- module(compiler, [compile_to_console/1, compile_file/2, compile_code/1, compile_to_list/2]).

:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(lexer).
:- use_module(compiler/statements).

% Re-export program for tests or other modules if needed
:- reexport(compiler/statements, [program//4]).

% --- THE COMPILER ENTRY POINT ---

% Helper to add a final END statement if needed
append_end(Lines, FinalLines) :-
    ( last(Lines, LastLineCode) ->
        atomic_list_concat([LastNumAtom|_], ' ', LastLineCode),
        atom_number(LastNumAtom, LastNum),
        EndLineNum is LastNum + 10
    ;   EndLineNum = 10
    ),
    atomic_list_concat([EndLineNum, ' END'], EndLine),
    append(Lines, [EndLine], FinalLines).

% Internal helper to get final compiled lines
compile_to_list(Source, FinalLines) :-
    tokenize(Source, Tokens),
    ( phrase(compiler:program(10, [], _, Lines), Tokens) ->
        append_end(Lines, FinalLines)
    ;   fail
    ).

% Option 1: Compile a raw string of CLB code to Console
compile_code(Source) :-
    ( compile_to_list(Source, FinalLines) ->
        atomic_list_concat(FinalLines, '\n', Final),
        format("~w~n", [Final])
    ;   format("ERROR: Parsing failed.~n")
    ).

% Option 2: Compile a file to Console
compile_to_console(Path) :-
    read_file_to_string(Path, Source, []),
    ( compile_to_list(Source, FinalLines) ->
        atomic_list_concat(FinalLines, '\n', Final),
        format("--- BASIC OUTPUT ---~n~w~n--------------------~n", [Final])
    ;   format("ERROR: Parsing failed in ~w~n", [Path])
    ).

% Option 3: Compile to File
compile_file(InPath, OutPath) :-
    read_file_to_string(InPath, Source, []),
    ( compile_to_list(Source, FinalLines) ->
        atomic_list_concat(FinalLines, '\n', Final),
        setup_call_cleanup(
            open(OutPath, write, Out),
            format(Out, "~w~n", [Final]),
            close(Out)
        ),
        format("Success: Compiled ~w to ~w~n", [InPath, OutPath])
    ;   format("ERROR: Parsing failed in ~w~n", [InPath])
    ).
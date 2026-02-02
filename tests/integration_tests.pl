:- module(integration_tests, []).
:- use_module('../src/compiler').

:- begin_tests(integration).

test(simple_program) :-
    % Read the source
    read_file_to_string('programs/simple.clb', Source, []),
    % Compile whole program to list of lines
    compiler:compile_to_list(Source, Lines),
    atomic_list_concat(Lines, '\n', Compiled),
    % Read the golden file
    read_file_to_string('tests/golden/simple.basic', ExpectedWithNewline, []),
    % Trim trailing newline if present for comparison
    atom_string(ExpectedAtom, ExpectedWithNewline),
    normalize_space(atom(Expected), ExpectedAtom),
    normalize_space(atom(Actual), Compiled),
    Actual == Expected.

:- end_tests(integration).

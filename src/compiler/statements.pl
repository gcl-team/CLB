:- module(compiler_statements, [program//4]).

:- use_module(library(lists)).
:- use_module('../mangler').
:- use_module(expressions).
:- use_module(utils).

% --- THE PARSER (DCG) ---
% program(CurrentLine, SymbolTableIn, SymbolTableOut, ListOfLines)
program(Line, _, _, _) -->
    { Line > 63999, !, format('FATAL ERROR: Line number ~w exceeds C64 limit of 63999.~n', [Line]), fail }.
program(Line, StateIn, StateOut, FinalLines) -->
    statement(Line, NextLine, StateIn, StateNext, Code), !,
    { 
        (is_list(Code) -> Lines = Code ; Lines = [Code])
    },
    program(NextLine, StateNext, StateOut, Rest),
    { append(Lines, Rest, FinalLines) }.
program(_, State, State, []) --> [].

% Rule: Type Name = Expr;
statement(Line, NextLine, StateIn, StateOut, FinalCode) -->
    [Type], { memberchk(Type, [int, bool, string]) }, !, [Name], ['='], expression(Expr, StateIn), [';'],
    { 
        NextLine is Line + 10,
        (member(Name-_, StateIn) -> 
            format('FATAL ERROR: Variable "~w" is already declared.~n', [Name]), fail 
        ;   true
        ),
        % Determine suffix based on type
        (Type = string -> Suffix = '$' ; Suffix = '%'),
        % Extract already used full BASIC names
        findall(Full, member(_-Full, StateIn), Used),
        mangle(Name, Used, Suffix, BasicVar),
        % Update Symbol Table
        append(StateIn, [Name-BasicVar], StateOut),
        atomic_list_concat([Line, ' ', BasicVar, ' = ', Expr], FinalCode)
    }.

% Rule: if (Condition) { Statements } [else { Statements }]
statement(Line, NextLine, StateIn, StateOut, [IfLine|FinalLines]) -->
    [if], !, ['('], expression(Cond, StateIn), [')'], ['{'],
    { IfBodyStart is Line + 10 },
    program(IfBodyStart, StateIn, IfStateOut, IfBlockLines),
    ['}'],
    ( [else] ->
        % ELSE PART
        {
            ( last(IfBlockLines, LastIfLine) ->
                atomic_list_concat([LastIfNum|_], ' ', LastIfLine),
                atom_number(LastIfNum, LastIfNumVal),
                IfGotoLine is LastIfNumVal + 10
            ;   IfGotoLine is Line + 10
            ),
            ElseBodyStart is IfGotoLine + 10
        },
        ['{'],
        program(ElseBodyStart, IfStateOut, StateOut, ElseBlockLines),
        ['}'],
        {
            ( last(ElseBlockLines, LastElseLine) ->
                atomic_list_concat([LastElseNum|_], ' ', LastElseLine),
                atom_number(LastElseNum, LastElseNumVal),
                NextLine is LastElseNumVal + 10
            ;   NextLine is ElseBodyStart
            ),
            atomic_list_concat([Line, ' IF NOT(', Cond, ') GOTO ', ElseBodyStart], IfLine),
            atomic_list_concat([IfGotoLine, ' GOTO ', NextLine], IfGotoCode),
            append(IfBlockLines, [IfGotoCode|ElseBlockLines], BlockLines),
            FinalLines = BlockLines
        }
    ;
        % NO ELSE PART
        {
            StateOut = IfStateOut,
            ( last(IfBlockLines, LastIfLine) ->
                atomic_list_concat([LastIfNum|_], ' ', LastIfLine),
                atom_number(LastIfNum, LastIfNumVal),
                NextLine is LastIfNumVal + 10
            ;   NextLine is Line + 10
            ),
            atomic_list_concat([Line, ' IF NOT(', Cond, ') GOTO ', NextLine], IfLine),
            FinalLines = IfBlockLines
        }
    ).

% Rule: while (Condition) { Statements }
statement(Line, NextLine, StateIn, StateOut, [WhileLine|FinalBodyLines]) -->
    [while], !, ['('], expression(Cond, StateIn), [')'], ['{'],
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
    [poke], !, ['('], [Addr], [','], [Val], [')'], [';'],
    {
        NextLine is Line + 10,
        resolve_val(Addr, State, AddrBasic),
        resolve_val(Val, State, ValBasic),
        atomic_list_concat([Line, ' POKE ', AddrBasic, ',', ValBasic], FinalCode)
    }.

% Rule: print("string"); or print(variable);
statement(Line, NextLine, State, State, FinalCode) -->
    [print], !, ['('], [Content], [')'], [';'],
    { 
        NextLine is Line + 10,
        resolve_val(Content, State, BasicContent),
        atomic_list_concat([Line, ' PRINT ', BasicContent], FinalCode) 
    }.

% Rule: clear();
statement(Line, NextLine, State, State, FinalCode) -->
    [clear], !, ['('], [')'], [';'],
    { 
        NextLine is Line + 10,
        atomic_list_concat([Line, ' PRINT CHR$(147)'], FinalCode) 
    }.

% Rule: Name = Expr; (Moved to end as it is most general)
statement(Line, NextLine, State, State, FinalCode) -->
    [Name], ['='], !, expression(Expr, State), [;],
    {
        NextLine is Line + 10,
        resolve_val(Name, State, BasicVar),
        atomic_list_concat([Line, ' ', BasicVar, ' = ', Expr], FinalCode)
    }.

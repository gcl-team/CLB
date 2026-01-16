:- module(lexer, [tokenize/2]).

% Entry point: Turns a string into a list of atoms/numbers
tokenize(String, Tokens) :-
    string_chars(String, Chars),
    phrase(tokens(Tokens), Chars).

% DCG Rules for Tokenizing
tokens([T|Ts]) --> 
    white_space, 
    token(T), !, 
    tokens(Ts).
tokens([]) --> white_space.

% Handle Strings in quotes: "HELLO"
token(String) --> 
    ['"'], string_content(Chars), ['"'], 
    { append(['"'|Chars], ['"'], FullChars), atom_chars(String, FullChars) }.

% Handle Symbols: ( ) , ; = + -
token(Symbol) --> 
    [C], { member(C, ['(', ')', ',', ';', '=', '+', '-', '<', '>']), atom_chars(Symbol, [C]) }.

% Handle Alpha-numeric words (keywords and variables)
token(Word) --> 
    [C], { char_type(C, alpha) }, 
    word_chars(Rest), 
    { atom_chars(Word, [C|Rest]) }.

% Handle Numbers
token(Number) --> 
    [C], { char_type(C, digit) }, 
    digit_chars(Rest), 
    { number_chars(Number, [C|Rest]) }.

% Helper to consume word characters
word_chars([C|Cs]) --> [C], { char_type(C, alnum) }, !, word_chars(Cs).
word_chars([]) --> [].

% Helper to consume digits
digit_chars([C|Cs]) --> [C], { char_type(C, digit) }, !, digit_chars(Cs).
digit_chars([]) --> [].

% Helper for strings
string_content([C|Cs]) --> [C], { C \= '"' }, !, string_content(Cs).
string_content([]) --> [].

% Consume white space
white_space --> [C], { char_type(C, space) }, !, white_space.
white_space --> [].
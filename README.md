# CLB: C-Like to BASIC V2 Transpiler

> A Prolog-powered transpiler for the Commodore 64.

## Overview

CLB is a tool that translates C-like code into BASIC V2 code suitable for execution on the Commodore 64. It leverages Prolog's powerful parsing and pattern-matching capabilities to facilitate the conversion process.

CLB allows a developer to write code with C# syntax (long variable names, curly brackets, explicit types) and compiles it into Commodore BASIC V2. It handles the "spaghetti logic" of line numbers and GOTO statements automatically.

## Quickstart

1. **Start Prolog**:
   ```bash
   swipl
   ```

2. **Load the Compiler**:
   ```prolog
   ?- [src/compiler].
   ```

3. **Compile Code**:
   ```prolog
   ?- compile_code("int score = 100; print(score);").
   ```
   
   **Output**:
   ```basic
   10 SC% = 100
   20 PRINT SC%
   ```

## Language Specification

| Feature | CLB Syntax | BASIC V2 Equivalent |
|---------|-------------|---------------------|
| Integers | `int x = 10;` | `10 x% = 10` |
| Strings | `string s = "Hello";` | `20 s$ = "Hello"` |
| Print Statement | `print(x);` | `30 PRINT x` |
| If Statement | `if (x > 5) { ... }` | `40 IF x > 5 THEN ...` |
| While Loop | `while (x < 10) { ... }` | `50 IF X >= 10 GOTO [END]` |
| Hardware Access | `poke(address, value);` | `60 POKE address, value` |
| Clear Screen | `clear();` | `70 PRINT CHR$(147)` |

### The Mangler Rules

To bypass the C64's 2-character variable limit:

1. Every variable is truncated to the first two letters;
2. If `playerScore` (PL) and `playerSpeed` (PL) collide, the second letter increments (PL -> PM);
3. The Mangler automatically avoids BASIC Reserved Words (e.g., it won't let a variable be named `TO` or `OR`).

## Technical Architecture

- Tokeniser: Uses Prolog to split source text into atoms;
- Parser (DCG): Validates C-style syntax and builds an Abstract Syntax Tree (AST);
- Symbol Table: A recursive list in Prolog that tracks LongName -> ShortName mappings;
- Generator: Flattens blocks into line-numbered code (increments of 10).

## Testing & CI/CD
We use SWI-Prolog plunit to ensure the compiler is "Truthful."

### Running Tests Locally

```bash
swipl -g "load_test_files([]), run_tests, halt." compiler.pl tests.pl

```

### GitHub Actions
The project includes a `.github/workflows/test.yml` to validate:

- Mangler Accuracy: No variable collisions or reserved word leaks;
- Parser Integrity: Correct conversion of `if/while` blocks into `GOTO` jumps.

## Roadmap


### Phase 1: The Core (Saturday AM)

- [ ] Implement the Mangler with collision detection;
- [ ] Build the Lexer to handle spaces, semicolons, and brackets;
- [ ] Create the Assignment rule (e.g., `int x = 5;`).

### Phase 2: Flow Control (Saturday PM)

- [ ] Implement if statements (requires calculating forward-jump line numbers);
- [ ] Implement while loops (requires back-jumping).

### Phase 3: Hardware Integration (Sunday)

- [ ] Export Prolog output to `.txt`;
- [ ] Use petcat to convert `.txt` to `.PRG`;
- [ ] The Moment of Truth: Load onto the real C64 and `RUN`.
    - Integer Suffix: Always map `int` to `%` in BASIC. It saves memory and is significantly faster on the 6510 CPU;
    - String Suffix: Always map `string` to `$` in BASIC;
    - Screen Width: Keep generated lines short; the C64 screen editor struggles with lines over 80 characters.
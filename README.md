# CLB: C-Like to BASIC V2 Transpiler

> A Prolog-powered transpiler for the Commodore 64.

## Overview

CLB is a tool that translates C-like code into BASIC V2 code suitable for execution on the Commodore 64. It leverages Prolog's powerful parsing and pattern-matching capabilities to facilitate the conversion process.

CLB allows a developer to write code with C# syntax (long variable names, curly brackets, explicit types) and compiles it into Commodore BASIC V2. It handles the "spaghetti logic" of line numbers and GOTO statements automatically.

## Quickstart

1.  **Build a Program**:
    ```bash
    ./simple.bash programs/simple.clb
    ```
    This will generate `build/simple.bas` (BASIC source) and `build/simple.prg` (C64 binary).

2.  **Run Tests**:
    ```bash
    swipl -g "use_module(tests/tests), run_all_tests, halt."
    ```

## Language Specification

| Feature | CLB Syntax | BASIC V2 Equivalent |
|---------|-------------|---------------------|
| Integers | `int x = 10;` | `10 x% = 10` |
| Strings | `string s = "Hello";` | `20 s$ = "Hello"` |
| Comparisons | `x == y`, `a != b`, `i < 10` | `x = y`, `a <> b`, `i < 10` |
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

- **Tokeniser**: Uses Prolog to split source text into atoms. Now supports multi-char symbols like `==`, `!=`, `>=`, `<=`.
- **Parser (DCG)**: Validates C-style syntax and builds an Abstract Syntax Tree (AST).
- **Symbol Table**: A recursive list in Prolog that tracks LongName -> ShortName mappings.
- **Generator**: Flattens blocks into line-numbered code (increments of 10).

## Testing & CI/CD
We use SWI-Prolog `plunit` and a **Golden Master** approach for integration testing.

### Running Tests Locally
```bash
swipl -g "use_module(tests/tests), run_all_tests, halt."
```

### GitHub Actions
The project includes a `.github/workflows/test.yml` to validate every commit:
- **Unit Tests**: Lexer, Mangler, and Compiler logic.
- **Integration Tests**: Compiles `programs/simple.clb` and compares against `tests/golden/simple.basic`.

## Compiler Learning Journey

This project serves as a practical refresher for Compiler Design theory.

| Lecture | Academic Topic | CLB Implementation | Status |
| :--- | :--- | :--- | :--- |
| **01** | **Lexical Analysis** | `lexer.pl`: Turning strings into atoms. | ✅ Done |
| **02** | **The Symbol Table** | `mangler.pl`: Solving the C64 2-char limit. | ✅ Done |
| **03** | **Context-Free Grammars** | `compiler.pl`: Using Prolog DCGs. | 🚧 In Progress |
| **04** | **Code Generation** | Flattening AST into BASIC line numbers. | 🚧 In Progress |
| **05** | **Control Flow** | Implementing `if` and `while` with `GOTO`. | ⏳ To Do |

## Roadmap

### Phase 1: The Core (Saturday AM)
- [x] Implement the Mangler with collision detection;
- [x] Build the Lexer to handle symbols, spaces, and strings;
- [x] Create the Assignment rule (e.g., `int x = 5;`).
- [x] Add logical comparison operators (`==`, `!=`, etc.)

### Phase 2: Flow Control (Saturday PM)
- [ ] Implement if statements (requires calculating forward-jump line numbers);
- [ ] Implement while loops (requires back-jumping).

### Phase 3: Hardware Integration (Sunday)
- [x] Create build script for petcat integration;
- [ ] Export Prolog output to `.txt`;
- [x] Use petcat to convert `.txt` to `.PRG`;
- [ ] The Moment of Truth: Load onto the real C64 and `RUN`.

---
**C64 Development Tips:**
- **Integer Suffix**: Always map `int` to `%` in BASIC. It saves memory and is significantly faster on the 6510 CPU.
- **Screen Width**: Keep generated lines short; the C64 screen editor struggles with lines over 80 characters.
# CLB: C-Like to BASIC V2 Transpiler

> A Prolog-powered transpiler for the Commodore 64.

## Overview

CLB is a tool that translates C-like code into BASIC V2 code suitable for execution on the Commodore 64. It leverages Prolog's powerful parsing and pattern-matching capabilities to facilitate the conversion process.

CLB allows a developer to write code with C-style syntax (long variable names, curly brackets, explicit types) and compiles it into Commodore BASIC V2. It handles the "spaghetti logic" of line numbers and GOTO statements automatically.

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
| Integers | `int x = 10;` | `10 X% = 10` |
| Booleans | `bool active = true;` | `20 AC% = -1` (0 for false) |
| Strings | `string s = "Hello";` | `30 S$ = "Hello"` |
| Comparisons | `==`, `!=`, `<`, `>`, `<=`, `>=` | `=`, `<>`, `<`, `>`, `<=`, `>=` |
| Logical | `&&`, `||`, `!` | `AND`, `OR`, `NOT` |
| If/Else | `if (cond) { ... } else { ... }` | `IF NOT(cond) GOTO [ELSE/END]` |
| While Loop | `while (cond) { ... }` | `IF NOT(cond) GOTO [EXIT] ... GOTO [START]` |
| Hardware Access| `poke(address, value);` | `POKE address, value` |
| Clear Screen | `clear();` | `PRINT CHR$(147)` |

### The Mangler Rules

To bypass the C64's 2-character variable limit while keeping C-style naming:

1. **Variables must have unique names**: The compiler blocks re-declaring the same name (e.g., you cannot have an `int player` and a `string player`).
2. **Prefix Coexistence**: Different variables like `playerScore` and `playerName` can coexist as `PL%` and `PL$` because BASIC V2 treats `%` and `$` as separate memory locations.
3. **Collision Resolution**: If two different variables of the **same type** share a prefix (e.g., `playerScore` and `playerSpeed` both want `PL%`), the second one increments to a new name (e.g., `PM%`).
4. **Reserved Word Avoidance**: The Mangler automatically avoids mapping to BASIC words like `TO`, `OR`, or `IF`.

## Technical Architecture

- **Tokeniser**: Uses Prolog DCGs to split source text into atoms. Supports multi-character symbols and quoted strings.
- **Parser (DCG)**: A recursive-descent parser that builds the logic flow. Now handles **Blocks** and **Jumps** for control flow.
- **Symbol Table**: Tracks `LongName -> BASIC_Name` mappings and ensures no variable redefinition (C-style scoping).
- **Generator**: Generates line-numbered BASIC V2 code, automatically appending an `END` statement to all programs for safety.

## Testing & CI/CD
We use SWI-Prolog `plunit` for unit testing and a **Golden Master** approach for integration testing.

### Running Tests Locally
```bash
swipl -g "use_module(tests/tests), run_all_tests, halt."
```

### GitHub Actions
The project includes a `.github/workflows/test.yml` to validate every commit:
- **Unit Tests**: Lexer, Mangler, and Compiler logic.
- **Integration Tests**: Compiles `programs/simple.clb` and compares against `tests/golden/simple.basic`.

## Compiler Learning Journey

| Lecture | Academic Topic | CLB Implementation | Status |
| :--- | :--- | :--- | :--- |
| **01** | **Lexical Analysis** | `lexer.pl`: Turning strings into atoms. | ✅ Done |
| **02** | **The Symbol Table** | `mangler.pl`: Type-aware name mangling. | ✅ Done |
| **03** | **Context-Free Grammars** | `compiler.pl`: DCG-based statement parsing. | ✅ Done |
| **04** | **Control Flow** | `if`, `else`, and `while` jump logic. | ✅ Done |
| **05** | **Optimization** | Constant folding & dead code removal. | ⏳ To Do |
| **06** | **Intermediate Rep** | Multi-pass AST for better optimization. | ⏳ To Do |

## Roadmap

### Phase 1: The Core (Completed)
- [x] Implement the Mangler with collision detection and type suffixes.
- [x] Build the Lexer for symbols, strings, and operators.
- [x] Support `int`, `bool`, and `string` types.
- [x] Implement logical and comparison operators.

### Phase 2: Flow Control (In Progress)
- [x] Implement `if` and `else` statements with forward jumps.
- [x] Implement `while` loops with back-jumping.
- [ ] Implement `for` loops (C-style).
- [ ] Implement `else if` support.

### Phase 3: Hardware & Polish
- [x] Robust build script (`simple.bash`) with `petcat` support.
- [x] Auto-generate `END` statement for all programs.
- [ ] Implement `input()` statement for player interaction.
- [ ] Implement `char` type (8-bit bytes).
- [ ] Add math shortcuts (`++`, `--`, `+=`, etc.).
- [ ] Implement Arrays (for maps, inventory, etc.).
- [ ] Implement Functions/Subroutines using `GOSUB`.
- [ ] Optimization: Auto-split long lines (>80 chars).

### Phase 4: The Optimizer (Multi-Pass)
- [ ] **Constant Folding**: Pre-calculate `20 + 30` into `50` at compile time.
- [ ] **Constant Propagation**: Replace variables with known constants in subsequent expressions.
- [ ] **Dead Code Elimination**: Remove unused variables and unreachable code blocks.
- [ ] **Tree Shaking**: Prune the generated BASIC code to only included used logic.

---
**C64 Development Tips:**
- **Integer Suffix (%):** Always use `int` for numbers. In BASIC V2, `%` saves memory and is significantly faster on the 6510 CPU.
- **Screen Width:** Keep generated lines short; the C64 screen editor struggles with lines over 80 characters. Our compiler aims to keep lines discrete.
# ANDYC - Andy's PL/0+ Compiler written in Common Lisp

This project is an implementation of a **PL/0 compiler** written in **Common Lisp**.
The goal is to study compiler construction by following Wirth’s classic PL/0 language, while exploring modern backends such as **WebAssembly (WASM)**.

## About PL/0

PL/0 is a small teaching language introduced by Niklaus Wirth in *Algorithms + Data Structures = Programs* (1976).

It has:
- Constants, variables, and procedures
- Arithmetic expressions and conditions
- Control flow with `if` and `while`
- Structured blocks using `begin … end`

It is deliberately minimal, making it ideal for educational compiler projects.

## Updates

- The language has been updated with comments (//) and types.

### Example Program

```pl0
// Example pl0 program
const int max = 10;

int x, y, sum;

// Procedure 'add' adds 2 integers
procedure add;
begin
   sum := x + y;
end;

// Main program block
begin
  x := 3;
  y := 5;
  call add;
end.
```

## Project Structure

```text

src/
  lexer.lisp     ; Tokenizer for PL/0 source code
  ast.lisp       ; Abstract syntax tree (AST) definitions
  parser.lisp    ; Recursive-descent parser for PL/0 grammar
  analyzer.lisp  : Symbol table generator and symantic checking
  emitter.lisp   ; Code generator for IR / WASM
  main.lisp      ; Main program driver

tests/
  *.pl0          ; Example PL/0 programs for testing
```

## Getting Started

### Requirements
- Common Lisp implementation (tested with **SBCL** and **CCL**)
- [Quicklisp](https://www.quicklisp.org/) package manager
- [SLIME](https://common-lisp.net/project/slime/) for Emacs integration (recommended)

### Building the Project

Clone the repository:

```bash
git clone https://github.com/YOURNAME/pl0-compiler.git
cd pl0-compiler
```
```lisp
(ql:quickload :pl0-compiler)
(pl0-compiler:parse-file "tests/example.pl0")
```

## References

Niklaus Wirth, Algorithms + Data Structures = Programs, 1976

The PL/0 Grammar

## More Information

For more information about the project, see project-notes.org.

## License
MIT License. See LICENSE for details.

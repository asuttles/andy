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

### Example Program

```pl0
const m = 7;
var x, y;

begin
  x := 1;
  y := 1;
  while y < m do
    begin
      x := x + y;
      y := y + 1
    end
end.
```

## Project Structure

```text

src/
  lexer.lisp     ; Tokenizer for PL/0 source code
  parser.lisp    ; Recursive-descent parser for PL/0 grammar
  ast.lisp       ; Abstract syntax tree (AST) definitions
  main.lisp      ; Main program driver
  wasm.lisp      ; Code generator for IR / WASM

tests/
  *.pl0          ; Example PL/0 programs for testing
```

## Getting Started

### Requirements
- Common Lisp implementation (tested with **SBCL** and **CCL**)
- [Quicklisp](https://www.quicklisp.org/) package manager
- [SLIME](https://common-lisp.net/project/slime/) or [Sly](https://github.com/joaotavora/sly) for Emacs integration (recommended)

### Building

Clone the repository:

```bash
git clone https://github.com/YOURNAME/pl0-compiler.git
cd pl0-compiler
```
```lisp
(ql:quickload :pl0-compiler)
(pl0-compiler:parse-file "tests/example.pl0")
```

## Roadmap

- Implement basic optimizations (constant folding, dead code elimination)
- Extend the language (I/O, strings, arrays, etc.)
- Interactive REPL for experimenting with PL/0 code

## References

Niklaus Wirth, Algorithms + Data Structures = Programs, 1976

The PL/0 Grammar

## License
MIT License. See LICENSE for details.

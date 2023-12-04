# scheme-rs

A simple interperter + compiler in Rust using the [LLVM](https://llvm.org/) toolkit and the [Inkwell](https://thedan64.github.io/inkwell/inkwell/index.html) crate. The interpreter/compiler supports a subset of the scheme programming language. Take a look at the programs folder for sample scheme programs that can be run using the interpreter/compiler. The best way to understand the interpreter/compiler implementation is via grokking the following two projects.

- [lisp-rs](https://github.com/vishpat/lisp-rs)
- [LLVM Basics](https://github.com/vishpat/llvm-basics)

Note: The compiler requires type hints in case of list and function object parameters or if the function returns a list. The interpreter ignores these type hints.

## Build
To build the compiler you will need the LLVM toolkit installed. The current version of the compiler uses LLVM 15. Easiest method to build and try out the compiler is run the project under Vscode using devcontainers. The devcontainer sets up the entire environment to build the compiler.

```bash
cargo build
```

## Testing

Run all of the tests

```
cargo tests
```

## REPL

```
cargo run
```

## Interpreter

To run programs using the interpreter

```
cargo run -- -i program.scm
```

## Compiler

To run programs using LLVM JIT compiler

```
cargo run -- -c program.scm
```

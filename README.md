# scheme-rs

A simple interperter + compiler in Rust using the [LLVM](https://llvm.org/) toolkit and the [Inkwell](https://thedan64.github.io/inkwell/inkwell/index.html) crate. The interpreter/compiler supports a subset of the scheme programming language. Take a look at the programs folder for sample scheme programs that can be run using the interpreter/compiler. The best way to understand the interpreter/compiler implementation is via grokking the following two books.

- [LISP Interpreter in Rust](https://www.amazon.com/Lisp-Interpreter-Rust-Vishal-Patil/dp/B0BFX1S2P7/ref=sr_1_1?crid=2YCAN0YJ4OAZ7&keywords=lisp+interpreter+in+rust&qid=1701608196&sprefix=Lisp+Inte%2Caps%2C181&sr=8-1)
- [LLVM API with Rust](https://www.amazon.com/LLVM-API-Rust-Vishal-Patil/dp/B0CGKVFFX5/ref=tmm_pap_swatch_0?_encoding=UTF8&qid=&sr=)

Note: The compiler requires type hints in case of list and function object parameters or if the function returns a list. The interpreter ignores these type hints.

## Build
To build the compiler you will need the LLVM toolkit installed. The current version of the compiler uses LLVM 15. Easiest method to build and try out the compiler is run the project under Vscode using devcontainers.

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

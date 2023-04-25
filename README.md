# scheme-rs

A simple interperter/compiler in Rust using the LLVM toolkit. The interpreter/compiler supports a subset of the scheme programming language. Take a look at the test modules for sample scheme programs that can be run using the interpreter/compiler.

## Build
To build the compiler you will need the LLVM toolkit installed. The current version of the compiler uses LLVM 15.

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

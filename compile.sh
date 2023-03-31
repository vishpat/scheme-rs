#!/bin/bash

export RUST_LOG=debug
set -x
cargo run -- -c programs/simple.scm
clang -o main main.ll
./main
echo $?

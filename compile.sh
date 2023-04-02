#!/bin/bash

rm -f main main.ll
export RUST_LOG=debug
set -x
cargo run -- -c programs/func.scm
clang -o main main.ll
./main
echo $?

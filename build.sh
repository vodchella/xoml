#!/usr/bin/env bash
set -e

mkdir -p build

ocamlopt -w +a-40-42-70 -I build -c -o build/common.cmx src/common.ml
ocamlopt -w +a-40-42-70 -I build -c -o build/input.cmx src/input.ml
ocamlopt -w +a-40-42-70 -I build -c -o build/board.cmx src/board.ml
ocamlfind ocamlopt -w +a-40-42-70 -package unix -I build -c -o build/xo.cmx src/xo.ml

ocamlfind ocamlopt -linkpkg -package unix \
  -I build \
  build/common.cmx \
  build/input.cmx \
  build/board.cmx \
  build/xo.cmx \
  -o build/xoml

#!/usr/bin/env bash
set -e

mkdir -p build

ocamlopt -g -w +a-40-42-70 -I build -c -o build/common.cmx src/common.ml
ocamlopt -g -w +a-40-42-70 -I build -c -o build/input.cmx src/input.ml
ocamlopt -g -w +a-40-42-70 -I build -c -o build/board.cmx src/board.ml
ocamlopt -g -w +a-40-42-70 -I build -c -o build/engine.cmx src/engine.ml
ocamlfind ocamlopt -g -w +a-40-42-70 -package unix -I build -c -o build/xo.cmx src/xo.ml

ocamlfind ocamlopt -linkpkg -package unix \
  -I build \
  build/common.cmx \
  build/input.cmx \
  build/board.cmx \
  build/engine.cmx \
  build/xo.cmx \
  -o build/xoml

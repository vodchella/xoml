#!/usr/bin/env bash
set -e

mkdir -p build

ocamlopt -I build -c -o build/common.cmx src/common.ml
ocamlopt -I build -c -o build/board.cmx src/board.ml
ocamlopt -I build -c -o build/input.cmx src/input.ml
ocamlfind ocamlopt -package unix -I build -c -o build/xo.cmx src/xo.ml

ocamlfind ocamlopt -linkpkg -package unix \
  -I build \
  build/common.cmx \
  build/board.cmx \
  build/input.cmx \
  build/xo.cmx \
  -o build/xoml

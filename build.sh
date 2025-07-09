mkdir -p build
ocamlopt -c src/xo.ml -o build/xo.cmx
ocamlopt -I build build/xo.cmx -o build/xoml

.PHONY: all run clean test

all:
	dune build

run:
	dune exec xoml

clean:
	dune clean

test:
	dune runtest -f

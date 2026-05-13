.PHONY: all run clean test

all:
	dune build

run:
	dune exec xoml -- $(ARGS)

clean:
	dune clean

test:
	dune runtest -f

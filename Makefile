.PHONY: ocaml-fmt

all: ocaml

ocaml:
	@dune build @install

format:
	@dune build @fmt --auto-promote

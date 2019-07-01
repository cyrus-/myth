debug:
	dune build src --profile dev

release:
	dune build src --profile release

deps:
	opam install dune core menhir js_of_ocaml tyxml 

open: release
	firefox _build/default/src/www/myth.html &

clean:
	dune clean


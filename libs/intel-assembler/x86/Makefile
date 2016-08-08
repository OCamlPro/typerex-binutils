all:
	../../../../_obuild/ocp-x86-gen/ocp-x86-gen.asm
	ocamlc -I .. -c intel_proc.mli intel_more.mli intel_assembler.ml
	rm -f *.cm?

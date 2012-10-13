.PHONY: demo
demo: variants
	./variants

variants: variants.ml
	ocamlopt -o variants variants.ml

.PHONY: clean
clean:
	rm -f *.cm[iox] *.o *~ variants

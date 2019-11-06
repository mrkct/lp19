rm *.cmo
rm *.cmi
ocamlc -c *.mli
ocamlc -c interval.ml
ocamlc -o main interval.cmo main.ml

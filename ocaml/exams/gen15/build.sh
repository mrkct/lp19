rm *.cmo
rm *.cmi
ocamlc -c NaturalI.mli
ocamlc -c natural.ml
ocamlc -o main natural.cmo main.ml
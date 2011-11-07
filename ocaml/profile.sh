#!/usr/bin/env sh

rm -f *.cmo
rm -f *.cmi
rm -f *.mli
rm -f MyBot.profile
ocamlcp -p a -g unix.cma ants.ml MyBot.ml -o MyBot.profile


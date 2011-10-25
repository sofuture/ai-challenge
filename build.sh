#!/usr/bin/env sh

cd ocaml
rm ants.mli
ocamlc -i ants.ml > ants.mli
ocamlbuild -libs unix MyBot.native
cd .. 

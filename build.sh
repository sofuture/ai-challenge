#!/usr/bin/env sh

cd ocaml

# clean crap up
rm -f MyBot.debug
rm -f MyBot.native
rm -f ants.mli
rm -f queue.mli
rm -rf _build/

# build
ocamlc -g -i queue.ml > queue.mli
ocamlc -g -i ants.ml | sed 's/in_channel/scanbuf/g' > ants.mli 
ocamlbuild -ocamlc "ocamlc -g" -libs unix MyBot.native

cd _build
ocamlc -g unix.cma ants.ml MyBot.ml -o MyBot.debug
ln -s _build/MyBot.debug ../MyBot.debug
cd ..
cd .. 

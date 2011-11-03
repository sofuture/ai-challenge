#!/usr/bin/env sh

cd ocaml
rm MyBot.debug
rm MyBot.native
rm ants.mli
rm queue.mli
rm -rf _build/
ocamlc -g -i queue.ml > queue.mli
ocamlc -g -i ants.ml | sed 's/in_channel/scanbuf/g' > ants.mli 
ocamlbuild -ocamlc "ocamlc -g" -libs unix MyBot.native

cd _build
ocamlc -g unix.cma ants.ml MyBot.ml -o MyBot.debug
ln -s _build/MyBot.debug ../MyBot.debug
cd ..
cd .. 

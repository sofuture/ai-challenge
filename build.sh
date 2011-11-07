#!/usr/bin/env sh

cd ocaml

# build
ocamlc -g -i queue.ml > queue.mli
ocamlc -g -i ants.ml | sed 's/in_channel/scanbuf/g' > ants.mli 
ocamlbuild -ocamlc "ocamlc -g" -libs unix -j 4  MyBot.native 

cd _build
ocamlc -g unix.cma ants.ml MyBot.ml -o MyBot.debug
ln -s _build/MyBot.debug ../MyBot.debug
cd ..
cd .. 

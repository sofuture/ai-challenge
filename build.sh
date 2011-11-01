#!/usr/bin/env sh

cd ocaml
rm ants.mli
rm queue.mli
ocamlc -i queue.ml > queue.mli
ocamlc -i ants.ml | sed 's/in_channel/scanbuf/g' > ants.mli 
ocamlbuild -libs unix MyBot.native
cd .. 

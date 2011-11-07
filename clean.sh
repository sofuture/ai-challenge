#!/usr/bin/env sh

cd ocaml

# build clean target
ocamlbuild -ocamlc "ocamlc -g" -libs unix -j 4  -clean MyBot.native 

cd .. 

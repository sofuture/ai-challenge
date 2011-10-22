#!/usr/bin/env sh

cd ocaml
ocamlbuild -libs unix MyBot.native
cd .. 

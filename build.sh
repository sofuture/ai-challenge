#!/usr/bin/env sh

cd ~/dev/ai-challenge/ocaml
ocamlbuild -libs unix MyBot.native
cd ~/dev/ai-challenge

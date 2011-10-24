#!/usr/bin/env sh

sh build.sh
python tcpclient.py ants.fluxid.pl 2081 "./ocaml/_build/MyBot.native" sofuture billy999 1

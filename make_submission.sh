#!/usr/bin/env sh

DATE=`date +"%Y%m%d%H%M%S"`

mkdir -p _deploy
cp ocaml/MyBot.ml _deploy/
cp ocaml/ants.ml _deploy/
cp ocaml/ants.mli _deploy/
cd _deploy
zip MyBot.zip MyBot.ml ants.ml ants.mli
mv MyBot.zip ../Bot-$DATE.zip
cd ..
rm -rf _deploy

#!/usr/bin/env sh

cd ~/dev/ai-challenge/ocaml
ocamlbuild -libs unix MyBot.native

cd ~/dev/ai-challenge
./tools/playgame.py --player_seed 42 \
--end_wait=0.25 --verbose \
--log_dir tools/game_logs \
--turns 100 \
--map_file tools/maps/multi_hill_maze/multi_maze_07.map \
"$@" \
"python tools/sample_bots/python/LeftyBot.py" \
"python tools/sample_bots/python/HunterBot.py" \
"./ocaml/MyBot.native" \
"./ocaml/MyBot.native" 

#!/usr/bin/env sh

sh build.sh

./tools/playgame.py --player_seed 42 \
--end_wait=0.25 --verbose \
--log_dir tools/game_logs \
--turns 100 \
--map_file tools/maps/multi_hill_maze/multi_maze_08.map \
"$@" \
"python tools/sample_bots/python/LeftyBot.py" \
"python tools/sample_bots/python/HunterBot.py" \
"python tools/sample_bots/python/GreedyBot.py" \
"./ocaml/MyBot.native" 

#!/usr/bin/env sh

sh build.sh

#--map_file tools/maps/multi_hill_maze/multi_maze_08.map 
#--map_file tools/maps/symmetric_random_walk/random_walk_01.map \

./tools/playgame.py --player_seed 42 \
--end_wait=0.25 --verbose \
--log_dir tools/game_logs \
--turntime=300 \
--turns 200 \
--strict \
--map_file tools/maps/random_walk_08p_02.map \
"./ocaml/MyBot.native" \
"python tools/sample_bots/python/LeftyBot.py" \
"python tools/sample_bots/python/HunterBot.py" \
"python tools/sample_bots/python/GreedyBot.py" \
"python tools/sample_bots/python/HoldBot.py" \
"python tools/sample_bots/python/RandomBot.py" \
"python tools/sample_bots/python/HunterBot.py" \
"python tools/sample_bots/python/GreedyBot.py" 

#!/usr/bin/env sh

sh build.sh

./tools/playgame.py --engine_seed 42 \
--player_seed 42 \
--food none \
--end_wait=0.25 \
--verbose \
--log_dir game_logs \
--turns 30 \
--map_file tools/submission_test/test.map \
"./ocaml/MyBot.native" \
"python tools/submission_test/TestBot.py" \
--nolaunch \
-e \
--strict \
--capture_errors

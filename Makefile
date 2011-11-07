all : build

build :
	sh build.sh

clean : clean-submission
	rm -rf ocaml/_build
	rm -f ocaml/MyBot.debug
	rm -f ocaml/MyBot.native

run :
	sh run.sh

test :
	sh test_bot.sh

logs :
	sh logs.sh

submit : submission

submission :
	sh make_submission.sh

clean-submission : 
	rm -f *.zip


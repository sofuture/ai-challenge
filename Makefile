all : build

build :
	sh build.sh

clean : clean-submission
	sh clean.sh

run :
	sh run.sh

test :
	sh test_bot.sh

log : logs

logs :
	sh logs.sh

submit : submission

submission :
	sh make_submission.sh

clean-submission : 
	rm -f *.zip


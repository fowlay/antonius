#
# Makefile
#
#
# It is assumed that the directory that contains this makefile
# has been set as the working directory.



HEADERS = $(wildcard include/*.hrl)
SOURCE = $(wildcard src/*.erl)


.PHONY: help test-quick test-long dialyze game-cli game-xboard bytecode hdeps clean


help:
	@echo "Targets in this makefile are:"
	@echo ""
	@echo "help           This help"
	@echo "test-quick     Quick tests"
	@echo "test-long      Time-consuming tests"
	@echo "dialyze        Check against type errors"
	@echo "game-cli       Play a game using the console"
	@echo "game-xboard    Play a game using xboard"
	@echo "clean          Remove most built objects"
	@echo "distclean      Remove all built objects"
	@echo ""



test-quick: bytecode
	bin/batchtest_SUITE.sh quickTests


test-long: bytecode
	bin/batchtest_SUITE.sh longTests


dialyze: var/dialyzer_plt
	dialyzer --plt var/dialyzer_plt --src -I include src


var/dialyzer_plt:
	dialyzer \
	  --build_plt \
	  --output_plt var/dialyzer_plt \
	  --apps erts kernel stdlib compiler common_test eunit test_server \
	|| true


game-cli: bytecode
	bin/play.sh


game-xboard: bytecode
	bin/xboard-wrapper.sh


bytecode: hdeps var/module.mk
	cd ebin && $(MAKE) all


var/module.mk: $(SOURCE)
	bin/deps.sh module.mk >$@


hdeps: var/header.mk
	cd hdeps && $(MAKE) all


var/header.mk: $(HEADER_FILES)
	bin/deps.sh header.mk >$@


clean:
	touch var/module.mk && cd ebin && $(MAKE) clean
	touch var/header.mk && cd hdeps && $(MAKE) clean
	cd var && $(MAKE) clean
	cd bin && $(MAKE) clean
	rm -f *~ .settings/*~


distclean:
	$(MAKE) clean
	rm -f var/dialyzer_plt
	rm -f var/log/*

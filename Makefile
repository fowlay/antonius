#
# Makefile
#
#
# It is assumed that the directory that contains this makefile
# has been set as the working directory.

# Subdirectories that have their own Makefile
SUBDIRS = test dialyze ebin hdeps lib tsrc var

HEADER_FILES = $(wildcard include/*.hrl)
SOURCE = $(wildcard src/*.erl)


.PHONY: \
  help \
  test \
  test-quick \
  test-long \
  test-perf \
  dialyze \
  game-cli \
  game-xboard \
  ics \
  game-ics \
  ebin \
  hdeps \
  lib \
  eclipse-setup \
  tsrc \
  clean \
  distclean \
  files


help:
	@echo "Targets in this makefile are:"
	@echo ""
	@echo "help           This help"
	@echo "test-quick     Quick tests"
	@echo "test-long      Time-consuming tests"
	@echo "test-perf      Performance tests"
	@echo "dialyze        Check against type errors"
	@echo "game-cli       Play a game in the console"
	@echo "game-xboard    Play a game using xboard"
	@echo "ics            Launch multiplayer server"
	@echo "game-ics       Play game using the server"
	@echo ""
	@echo "eclipse-setup  Adapt Eclipse C project setup to current Erlang environment"
	@echo ""
	@echo "tsrc           Rebuild the tests tarball"
	@echo "clean          Remove most built objects"
	@echo "distclean      Remove all built objects"
	@echo "files          Report all files"
	@echo ""




test-quick: ebin lib test
	bin/batchtest_SUITE.sh quickTests


test-long: ebin lib test
	bin/batchtest_SUITE.sh longTests


test-perf: ebin lib test
	bin/batchtest_SUITE.sh performanceTests


test:
	$(MAKE) -C $@


dialyze:
	$(MAKE) -C $@


game-cli: ebin lib
	bin/play.sh


game-xboard: ebin lib
	bin/xboard-wrapper.sh


ics: ebin lib
	bin/ics-wrapper.sh


game-ics: ebin lib
	bin/xboard-wrapper.sh -m multi


ebin: hdeps ebin/module.mk
	$(MAKE) -C $@ all


ebin/module.mk: $(SOURCE)
	bin/deps.sh $@ >$@


hdeps: hdeps/header.mk
	$(MAKE) -C $@ all


hdeps/header.mk: $(HEADER_FILES)
	bin/deps.sh $@ >$@


lib:
	$(MAKE) -C $@


tsrc:
	$(MAKE) -C $@


eclipse-setup:
	@echo -n "Close down Eclipse, then push Enter: " && read _
	@bin/eclipse-setup.sh


clean distclean:
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir $@; done
	rm -f erl_crash.dump

files:
	(find . -maxdepth 1 -type f |sed -e 's/^..//'; find [a-z]* -mindepth 1 -type f >&1) | sort

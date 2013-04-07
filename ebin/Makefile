#
# ebin/Makefile
#
# Builds all bytecode files, one per source file.
#
# The ../var/module.mk file contains all dependencies
# on header files.


.PHONY: all clean


include ../var/module.mk


SOURCE = $(wildcard ../src/*.erl)
BYTECODE = $(SOURCE:../src/%.erl=%.beam)


$(BYTECODE): %.beam: ../src/%.erl
	erlc -I ../include $<


all: $(BYTECODE)


clean:
	rm -f *.beam

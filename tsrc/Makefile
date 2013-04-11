#
# tsrc/Makefile
#
# This directory holds the test source as a .tar.bz2
# file.


SOURCE = $(wildcard ../test/*/*)


.PHONY: clean distclean


# To be run when the set of tests has been modified.

test.tar.bz2: $(SOURCE)
	$(MAKE) clean
	tar -cjf $@ -C ../test [a-z]*


clean:
	rm -f ../test/*/*~


distclean: clean

#
# lib/Makefile
#

.PHONY: all clean distclean


all: core_nif.so


core_nif.so: ../csrc/core_nif.c
	ErlangCompiler=`bash -c "command -v erlc"` \
	&& ErlangCompilerAbsPath=`readlink -f $$ErlangCompiler` \
	&& OtpDir=`dirname $$ErlangCompilerAbsPath`/.. \
	&& gcc -std=gnu99 -Wall -I$$OtpDir/usr/include -O3 -fPIC -shared -o $@ $<


clean:
	rm -f *.so


distclean: clean

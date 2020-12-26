#
# dialyze/Makefile
#


.PHONY: all clean distclean


all: dialyzer_plt
	dialyzer --plt dialyzer_plt --src -I ../include ../src


dialyzer_plt:
	dialyzer \
	  --build_plt \
	  --output_plt $@ \
	  --apps erts kernel stdlib compiler common_test eunit \
	|| true


clean:
	true


distclean: clean
	rm -f dialyzer_plt

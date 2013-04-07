#! /bin/bash


declare -r ScriptDir=`dirname $0`

mkdir -p /dev/shm/$USER
declare -r Out=/dev/shm/$USER/dialyze-`date +%s`


if [[ ! -r ~/.dialyzer_plt ]]; then
  echo "The file ~/.dialyzer_plt was not found. Please run"
  echo ""
  echo "    dialyzer --build_plt --apps erts kernel stdlib compiler common_test"
  echo ""
  echo "and retry."
  echo ""
  exit
fi



cd $ScriptDir/../src

dialyzer --verbose --src . >$Out

echo "Raw output ==================================="
cat $Out


echo ""
echo "Filtered output =============================="
cat $Out \
| sed \
    -e 's|  Checking whether the PLT /home/erarafo/.dialyzer_plt is up-to-date... yes|.|' \
    -e 's|  Compiling some key modules.*|.|' \
    -e 's|  Proceeding with analysis...|.|' \
    -e 's|Unknown functions:|.|' \
    -e 's|  eunit:test/1|.|' \
    -e 's|  eprof:analyze/0|.|' \
    -e 's|  eprof:log/1|.|' \
    -e 's|  eprof:start/0|.|' \
    -e 's|  eprof:start_profiling/1|.|' \
    -e 's|  eprof:stop/0|.|' \
    -e 's|  eprof:stop_profiling/0|.|' \
    -e 's|  ct:get_config/1|.|' \
    -e 's|done (passed successfully)|.|' \
    -e 's|done (warnings were emitted)|.|' \

rm $Out

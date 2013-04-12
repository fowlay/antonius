#! /bin/bash



declare -r ScriptDir=$0

cd $ScriptDir/..

declare -r Root=`pwd`
declare -r BytecodeDir=$Root/ebin
declare -r LibDir=$Root/lib

erl \
  -noshell \
  -pa $BytecodeDir \
  -run xbi_controller start $LibDir xboard \
  -run init stop

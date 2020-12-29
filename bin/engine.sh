#! /bin/bash

declare -r ScriptDir=$(dirname $0)

cd $ScriptDir/..

declare -r Root=$(pwd)
declare -r BytecodeDir=$Root/ebin
declare -r LibDir=$Root/lib

erl \
  -noshell \
  -pa $BytecodeDir \
  -run xbi_controller start $LibDir xboard null null \
  -run init stop

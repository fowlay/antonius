#! /bin/bash
#
# Wrapper for ct_run
#
# This wrapper can be used for executing all tests
# in the tests directory. A config file is used to
# pass the path of the tests directory.

declare -r ConfigFile=var/config-`date +%s`
declare -r ResultFile=var/erlang.txt
declare -r HtmlDir=var/html

declare -r ScriptDir=`dirname $0`

cd $ScriptDir/..

declare -r LibDir=lib
declare -r TestDir=test
declare -r BytecodeDir=ebin

{ \
  echo '{libDir, "'`readlink -f $LibDir`'"}.'; \
  echo '{testDir, "'`readlink -f $TestDir`'"}.'; \
  echo '{resultFile, "'`readlink -f $ResultFile`'"}.'; \
} >$ConfigFile

mkdir -p $HtmlDir

ct_run \
  -config $ConfigFile \
  -logdir $HtmlDir \
  -no_auto_compile \
  -suite $BytecodeDir/test_SUITE \
  -group $* \
  -pa $BytecodeDir

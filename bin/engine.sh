#! /bin/bash

declare -r Root=$HOME/antonius
declare -r Bytecodes=$Root/ebin

erl \
  -noshell \
  -pa $Bytecodes \
  -run xbi_controller start arg1 \
  -run erlang halt

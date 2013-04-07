#! /bin/bash
#
#  -smp auto is the default, expect it to work
#
declare -r ScriptDir=`dirname $0`

cd $ScriptDir/..

if [[ ! -r ebin/cli_game.beam ]]; then
  echo "Missing .beam file: cli_game.beam"
  exit
fi

cat <<'EOF'

Antonius 1.0
Copyright (C) 2013 Rabbe Fogelholm
This program comes with ABSOLUTELY NO WARRANTY. This is free
software, and you are welcome to redistribute it under certain
conditions. See http://www.gnu.org/licenses/gpl-3.0.html for 
details on warranty and redistribution.

Type 'help' for a list of commands. Type 'setup' to set up the
pieces. Type e g 'move d2 d4' for your first move. Type 'play'
to let black make a move.

EOF

erl \
  -pa ebin \
  -noshell \
  -run cli_game initVM \
  -run cli_game main run \
  -run erlang halt

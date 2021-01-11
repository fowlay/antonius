#! /bin/bash
#
#  Starts a multiplayer service.


declare -r IcsPortDefault=5000
declare -r ControlPortDefault=5090

declare -r ScriptDir=`dirname $0`
cd $ScriptDir/..
declare Root=`pwd`
declare -r Pgm=ics-wrapper.sh

mkdir -p $Root/var/log
declare -r Log=$Root/var/log/ics-$$.log

declare -r DepthDefault=3

die() {
  echo "$Pgm: $1" >&2
  exit 1
}


help() {
  echo "Options are:"
  echo "  -D DEPTH   set the analysis depth (1, 2, 3, 4, 5, 6, ..., defaults to $DepthDefault)"
  echo "  -h         this help"
  echo ""
}


declare Depth=$DepthDefault
while getopts ':hD:' OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    D)
      Depth=$OPTARG;;
    *)
      die "unknown option, try -h for help"
  esac
done




rm -rf $Log
touch $Log


# TODO, only while developing
xterm -e tail -f -n 1000 $Log &

erl \
    -noshell \
    -pa $Root/ebin \
    -run ics_main start $IcsPortDefault $ControlPortDefault $Log

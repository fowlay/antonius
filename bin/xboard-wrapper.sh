#! /bin/bash
#
declare -r Pgm=xboard-wrapper.sh
#
#
#  Starts xboard and sets up a window for debug output
#  from the engine


declare -r ScriptDir=`dirname $0`
cd $ScriptDir/..
declare Root=`pwd`

mkdir -p $Root/var/log
declare -r Log=$Root/var/log/xboard-debug-$$.log


declare -r EngineDefault=$Root/bin/engine.sh
declare -r DepthDefault=3


die() {
  echo "$Pgm: $1" >&2
  exit 1
}


help() {
  echo "Options are:"
  echo "  -D DEPTH   set the analysis depth (1, 2, 3, 4, 5, 6, ..., defaults to $DepthDefault)"
  echo "  -e ENGINE  set the chess engine (defaults to $EngineDefault)"
  echo "  -h         this help"
  echo ""
  echo "A non-default chess engine may be given as an absolute path of the"
  echo "executable, or a path relative to $Root."
}


declare Engine=$EngineDefault
declare Depth=$DepthDefault
while getopts ':hD:e:' OPT; do
  case "$OPT" in
    h)
      help
      exit;;
    e)
      Engine="$OPTARG";;
    D)
      Depth=$OPTARG;;
    *)
      die "unknown option, try -h for help"
  esac
done



if [[ -z "$Engine" ]]; then
  die "engine must be specified, -h for help"
elif [[ ! -x "$Engine" ]]; then
  die "cannot execute engine: $Engine"
fi


rm -rf $Log
touch $Log


xterm -e tail -f -n 1000 $Log &


# The -initString option is awkward. The default setting is
# new\nrandom\n but we reset it to just new\n. The option value
# must include a terminating newline, hence the quotes appearing
# on separate lines.

#   -ponderNextMove false \
#   -xthinking \


xboard \
  -fcp "$Engine" \
  -debugMode true \
  -engineDebugOutput 1 \
  -nameOfDebugFile $Log \
  -initString \
"new
sd $Depth
"

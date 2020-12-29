#! /bin/bash

# Start xboard with an engine (or proxy) backend

set -o nounset
set -o errexit

declare -r Pgm=$(basename $0)
declare -r ScriptDir=`dirname $0`
cd $ScriptDir/..
declare Root=`pwd`

mkdir -p $Root/var/log
declare -r Log=$Root/var/log/xboard-debug-$$.log


declare -r ModeDefault=single
declare -r EngineDefault=$Root/bin/engine.sh
declare -r DepthDefault=3

declare -r ServerHostDefault=localhost
declare -r ServerPortDefault=5000

die() {
  echo "$Pgm: $1" >&2
  exit 1
}


help() {
    echo "Options are:"
    echo "  -m MODE    Engine mode: single|multi, defaults to $ModeDefault"
    echo "  -e ENGINE  Engine when MODE=single, defaults to $EngineDefault"
    echo "  -s HOST    Server host when MODE=multi, defaults to $ServerHostDefault"
    echo "  -p PORT    Server port when MODE=multi, defaults to $ServerPortDefault"
    echo "  -D DEPTH   set the analysis depth (1, 2, 3, 4, 5, 6, ..., defaults to $DepthDefault)"
    echo "  -h         this help"
}

declare Mode=$ModeDefault
declare Engine=$EngineDefault
declare ServerHost=$ServerHostDefault
declare ServerPort=$ServerPortDefault
declare Depth=$DepthDefault
while getopts ':m:e:s:p:D:h' OPT; do
    case "$OPT" in
	m)
	    Mode="$OPTARG";;
	e)
	    Engine="$OPTARG";;
	s)
	    ServerHost="OPTARG";;
	p)
	    ServerPort="OPTARG";;
	D)
	    Depth=$OPTARG;;
	h)
	    help
	    exit;;
	*)
	    die "unknown option, try -h for help"
    esac
done


case "$Mode" in
    single|multi)
	true;;
    *)
	die "unknown mode: $Mode"
esac

if [[ -z "$Engine" ]]; then
  die "engine must be specified, -h for help"
elif [[ ! -x "$Engine" ]]; then
  die "cannot execute engine: $Engine"
fi


rm -rf $Log
touch $Log


xterm -e tail -f -n 1000 $Log &


if [[ $Mode == single ]]; then

    xboard \
	-fcp "$Engine" \
	-debugMode true \
	-engineDebugOutput 1 \
	-nameOfDebugFile $Log \
	-initString \
	"new
sd $Depth
"

elif [[ $Mode == multi ]]; then

    export ServerHost=$ServerHost
    export ServerPort=$ServerPort
    xboard \
	-fcp $Root/bin/proxy.sh \
	-debugMode true \
	-engineDebugOutput 1 \
	-nameOfDebugFile $Log \
        -initString \
	"new
sd $Depth
"
	
fi

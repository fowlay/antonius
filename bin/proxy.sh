#! /bin/bash

# Argumentless wrapper

declare -r ScriptDir=$(dirname $0)
declare -r Root=$(readlink -f $ScriptDir/..)
declare -r EbinDir=$Root/ebin
declare -r LogDir=$Root/var/log

Log=$(mktemp $LogDir/proxy-XXXXXXXX)

echo "proxy wrapper launched" >$Log
echo "proxy ServerHost: $ServerHost" >>$Log
echo "proxy ServerPort: $ServerPort" >>$Log

erl \
    -noshell \
    -pa $EbinDir \
    -run xbp_downstream start $Log $ServerHost $ServerPort

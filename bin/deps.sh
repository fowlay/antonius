#! /bin/bash
#
#

case "$1" in

ebin/module.mk)

  for f in src/*.erl; do
    b=`basename $f .erl`
    d=`sed \
	 -e '/^-include_lib/d' \
	 -e '/^-include/!d' \
	 -e 's|-include("||' \
	 -e 's|").*||' \
	 -e 's|\.hrl|.t|' \
         -e 's|^|../hdeps/|' \
	 $f`
    echo $b.beam: ../$f $d
    echo ""
  done;;

hdeps/header.mk)

  for f in include/*.hrl; do
    b=`basename $f .hrl`
    d=`sed \
	 -e '/^-include/!d' \
	 -e 's|-include("||' \
	 -e 's|").*||' \
	 -e 's|\.hrl|.t|' \
	 $f`
    echo $b.t: ../$f $d
    echo ""
  done;;

*)
  echo "unknown argument: $1" >&2
  exit 1

esac

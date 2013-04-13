#! /bin/bash
#
# Set up the .cproject file

declare -r ScriptDir=`dirname $0`

declare -r ErlangCompiler=`command -v erlc`

if [[ -z "$ErlangCompiler" ]]; then
  echo "FATAL: cannot locate the Erlang compiler"
  exit 1
else
  declare -r ErlangCompilerAbsPath=`readlink -f "$ErlangCompiler"`
  declare -r ErlangBinDir=`dirname "$ErlangCompilerAbsPath"`
  declare -r OtpDir=`dirname "$ErlangBinDir"`
  declare -r ErlangIncludeDir=$OtpDir/usr/include

  if [[ ! -d "$ErlangIncludeDir" ]]; then
    echo "FATAL: cannot locate the Erlang include directory"
    exit 1
  fi

  sed \
    -i \
    -e '/<stringMacro name="ERLANG_INCLUDE"/s|value=".*"|value="'$ErlangIncludeDir'"|' \
    $ScriptDir/../eclipse/nif/.cproject

  echo "inserted ERLANG_INCLUDE=$ErlangIncludeDir in Eclipse .cproject file"
fi

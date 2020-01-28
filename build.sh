#!/usr/bin/env bash

set -eu
set -o pipefail

cd `dirname $0`

FSIARGS=""
OS=${OS:-"unknown"}
if [[ "$OS" != "Windows_NT" ]]
then
  FSIARGS="--fsiargs -d:MONO"
  export FrameworkPathOverride
  FrameworkPathOverride=$(dirname "$(command -v mono)")/../lib/mono/4.5/
fi

function run() {
  if [[ "$OS" != "Windows_NT" ]]
  then
    mono "$@"
  else
    "$@"
  fi
}

if [[ "$OS" != "Windows_NT" ]] &&
       [ ! -e ~/.config/.mono/certs ]
then
  mozroots --import --sync --quiet
fi

dotnet tool restore
run .paket/paket.exe restore

dotnet fake run "$@"


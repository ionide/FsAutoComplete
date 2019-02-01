#!/usr/bin/env bash

# If FORCE_DOTNET_VERSION has been setted, update global.json
if [[ ! -z $FORCE_DOTNET_VERSION ]]; then
    sed -i '' -e 's,"version": ".*"$,"version": "'$FORCE_DOTNET_VERSION'",g' global.json;
fi

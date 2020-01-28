#!/usr/bin/env bash



dotnet tool restore
dotnet restore

dotnet fake run "$@"


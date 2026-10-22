#!/usr/bin/env bash
# Lists the dotnet/fsharp commits in <from>...<to> that touch the checker: src/Compiler/Service/ and
# src/Compiler/Utilities/Caches.*. One line per commit: short sha | title (#PR) | the files it touches there.
#
# Usage: service-commits.sh <from> <to>
#   <to> can be a branch, e.g. main for the commits not yet in a nightly.
set -euo pipefail

from=$1
to=$2

gh api "repos/dotnet/fsharp/compare/$from...$to" --jq '.commits[].sha' |
  while read -r sha; do
    gh api "repos/dotnet/fsharp/commits/$sha" --jq '
      [ .sha[0:9],
        (.commit.message | split("\n")[0]),
        ([.files[].filename | select(test("^src/Compiler/(Service/|Utilities/Caches)"))]
          | map(split("/") | last) | unique | join(" ")) ]
      | select(.[2] != "")
      | join(" | ")'
  done

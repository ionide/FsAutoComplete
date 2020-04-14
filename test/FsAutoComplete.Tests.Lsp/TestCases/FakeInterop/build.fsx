#r "paket:
nuget FSharp.Core 4.7.0
nuget Fake.Core.Target prerelease //"

#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core

// Default target
Target.create "Default" (fun _ ->
  Trace.trace "Hello World from FAKE"
)

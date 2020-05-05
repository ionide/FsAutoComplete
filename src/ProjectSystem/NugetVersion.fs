[<RequireQualifiedAccess>]
module ProjectSystem.NugetVersion

open System

[<NoComparison>]
type NugetVersion = NugetVersion of major: int * minor: int * build: int * suffix: string
with
  override x.ToString() =
    match x with
    | NugetVersion(major, minor, build, suffix) when String.IsNullOrWhiteSpace suffix -> sprintf "%d.%d.%d" major minor build
    | NugetVersion(major, minor, build, suffix) -> sprintf "%d.%d.%d-%s" major minor build suffix

/// custom comparison for these versions means that you compare major/minor/build,
/// but an empty preview string marks a stable version, which is greater
let compareNugetVersion (NugetVersion(lM, lm, lb, ls)) (NugetVersion(rM, rm, rb, rs)) =
  match compare lM rM with
  | 0 ->
    match compare lm rm with
    | 0 ->
      match compare lb rb with
      | 0 ->
        match ls, rs with
        | "", s -> 1 // no preview string means left is a stable release and so is greater than right
        | s, "" -> -1 // no preview string means right is a stable release and so is greater than left
        | ls, rs -> compare ls rs // no shortcut means compare lexigrapically
      | n -> n
    | n -> n
  | n -> n

/// Parse nuget version strings into numeric and suffix parts
///
/// Format: `$(Major).$(Minor).$(Build) [-SomeSuffix]`
let deconstructVersion (version: string): NugetVersion =
  let version, suffix =
    let pos = version.IndexOf("-")
    if pos >= 0
    then
      version.Substring(0, pos), version.Substring(pos + 1)
    else
      version, ""

  let elements = version.Split('.')

  if elements.Length < 3 then
      NugetVersion(0, 0, 0, suffix)
  else
      NugetVersion(Int32.Parse(elements.[0]), Int32.Parse(elements.[1]), Int32.Parse(elements.[2]), suffix)

let tryDeconstructVersion (version: string): NugetVersion option =
  try
    Some (deconstructVersion version)
  with
  | _ -> None

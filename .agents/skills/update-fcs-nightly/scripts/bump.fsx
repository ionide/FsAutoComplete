// Moves the FcsNightly paket group and Directory.Build.props to a newer FCS from the dotnet/dotnet feeds.
//
// Usage: dotnet fsi bump.fsx [--dry-run] [version]
//   version    an exact FSharp.Compiler.Service version; the newest one on the feeds when left out
//   --dry-run  print what would change, and edit nothing
//
// Prints the FCS and FSharp.Core versions, the feed, and the dotnet/fsharp commit range between the
// current and the new version. It does not run paket: run `dotnet paket install` afterwards.

open System
open System.IO
open System.Net.Http
open System.Text.Json
open System.Text.RegularExpressions

// scripts -> update-fcs-nightly -> skills -> .agents -> repository root
let repositoryRoot =
  Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "..", ".."))

let paketDependencies = Path.Combine(repositoryRoot, "paket.dependencies")
let buildProps = Path.Combine(repositoryRoot, "Directory.Build.props")

/// The feeds the VMR publishes to. A new major appears when dotnet/dotnet main moves to the next .NET.
let feeds = [ "dotnet11"; "dotnet12"; "dotnet13"; "dotnet14" ]

let feedIndex feed =
  $"https://pkgs.dev.azure.com/dnceng/public/_packaging/%s{feed}/nuget/v3/index.json"

let flatContainer feed = $"https://pkgs.dev.azure.com/dnceng/public/_packaging/%s{feed}/nuget/v3/flat2"

let http = new HttpClient()

let tryGetString (url: string) =
  use response = http.GetAsync(url).Result

  if response.IsSuccessStatusCode then
    Some(response.Content.ReadAsStringAsync().Result)
  else
    None

/// SemVer 2 order: release after prerelease, numeric identifiers compared as numbers.
let compareVersions (a: string) (b: string) =
  let split (v: string) =
    match v.Split('-', 2) with
    | [| core; pre |] -> core, Some pre
    | _ -> v, None

  let compareIdentifier (x: string) (y: string) =
    match Int64.TryParse x, Int64.TryParse y with
    | (true, x), (true, y) -> compare x y
    | (true, _), _ -> -1
    | _, (true, _) -> 1
    | _ -> String.CompareOrdinal(x, y)

  let compareIdentifiers (xs: string array) (ys: string array) =
    Seq.zip xs ys
    |> Seq.map (fun (x, y) -> compareIdentifier x y)
    |> Seq.tryFind ((<>) 0)
    |> Option.defaultValue (compare xs.Length ys.Length)

  let coreA, preA = split a
  let coreB, preB = split b

  match compareIdentifiers (coreA.Split '.') (coreB.Split '.') with
  | 0 ->
    match preA, preB with
    | None, None -> 0
    | None, Some _ -> 1
    | Some _, None -> -1
    | Some x, Some y -> compareIdentifiers (x.Split '.') (y.Split '.')
  | c -> c

let fcsVersions feed =
  tryGetString $"%s{flatContainer feed}/fsharp.compiler.service/index.json"
  |> Option.map (fun json ->
    use doc = JsonDocument.Parse json

    [
      for v in doc.RootElement.GetProperty("versions").EnumerateArray() -> feed, v.GetString()
    ])
  |> Option.defaultValue []

let nuspec feed fcsVersion =
  tryGetString $"%s{flatContainer feed}/fsharp.compiler.service/%s{fcsVersion}/fsharp.compiler.service.nuspec"
  |> Option.defaultWith (fun () -> failwith $"No nuspec for FCS %s{fcsVersion} on %s{feed}")

/// FCS pins FSharp.Core to the exact version built with it.
let fsharpCoreVersion (nuspec: string) =
  Regex.Match(nuspec, """<dependency id="FSharp.Core" version="\[([^,\]]+)""").Groups[1].Value

/// The dotnet/fsharp commit in the VMR commit the package was built from.
let fsharpCommit (nuspec: string) =
  let vmrCommit =
    Regex.Match(nuspec, """<repository [^>]*commit="([0-9a-f]+)""").Groups[1].Value

  tryGetString $"https://raw.githubusercontent.com/dotnet/dotnet/%s{vmrCommit}/src/source-manifest.json"
  |> Option.bind (fun json ->
    use doc = JsonDocument.Parse json

    doc.RootElement.GetProperty("repositories").EnumerateArray()
    |> Seq.tryFind (fun r -> r.GetProperty("path").GetString() = "fsharp")
    |> Option.map (fun r -> r.GetProperty("commitSha").GetString()))

let currentFcsVersion () =
  Regex.Match(File.ReadAllText buildProps, "<FcsNightlyVersion>([^<]+)</FcsNightlyVersion>").Groups[1].Value

let replace (pattern: string) (replacement: string) (path: string) =
  let text = File.ReadAllText path

  if not (Regex.IsMatch(text, pattern, RegexOptions.Multiline)) then
    failwith $"Pattern '%s{pattern}' did not match in %s{path}"

  File.WriteAllText(path, Regex.Replace(text, pattern, replacement, RegexOptions.Multiline))

let args = fsi.CommandLineArgs |> Array.skip 1 |> List.ofArray
let dryRun = List.contains "--dry-run" args
let requested = args |> List.tryFind (fun a -> a.Length > 0 && Char.IsDigit a[0])

if not (File.Exists paketDependencies && File.Exists buildProps) then
  failwith $"No paket.dependencies and Directory.Build.props in %s{repositoryRoot}"

let candidates = feeds |> List.collect fcsVersions

let feed, fcsVersion =
  match requested with
  | Some v ->
    candidates
    |> List.tryFind (fun (_, c) -> c = v)
    |> Option.defaultWith (fun () -> failwith $"FCS %s{v} is on none of the feeds %A{feeds}")
  | None ->
    candidates
    |> List.sortWith (fun (_, a) (_, b) -> compareVersions a b)
    |> List.last

let previous = currentFcsVersion ()
let spec = nuspec feed fcsVersion
let coreVersion = fsharpCoreVersion spec

if not dryRun then
  // Only the FcsNightly group pins exact versions; its lines are indented.
  paketDependencies
  |> replace
    @"^(\s+source https://pkgs\.dev\.azure\.com/dnceng/public/_packaging/)dotnet\d+(/nuget/v3/index\.json)[ \t]*$"
    $"${{1}}%s{feed}${{2}}"

  paketDependencies
  |> replace @"^(\s+nuget FSharp\.Compiler\.Service )\S+[ \t]*$" $"${{1}}%s{fcsVersion}"

  paketDependencies
  |> replace @"^(\s+nuget FSharp\.Core )\S+[ \t]*$" $"${{1}}%s{coreVersion}"

  buildProps
  |> replace "<FcsNightlyVersion>[^<]+</FcsNightlyVersion>" $"<FcsNightlyVersion>%s{fcsVersion}</FcsNightlyVersion>"

  buildProps
  |> replace
    "<FSharpCoreNightlyVersion>[^<]+</FSharpCoreNightlyVersion>"
    $"<FSharpCoreNightlyVersion>%s{coreVersion}</FSharpCoreNightlyVersion>"

let verb = if dryRun then "would move" else "moved"

if previous = fcsVersion then
  printfn $"FCS %s{fcsVersion} is already the newest (FSharp.Core %s{coreVersion}, %s{feedIndex feed})"
else
  printfn $"FCS %s{verb} %s{previous} -> %s{fcsVersion} (FSharp.Core %s{coreVersion}) from %s{feedIndex feed}"

let previousCommit =
  if previous = "" || previous = fcsVersion then
    None
  else
    candidates
    |> List.tryFind (fun (_, v) -> v = previous)
    |> Option.bind (fun (f, v) -> fsharpCommit (nuspec f v))

match previousCommit, fsharpCommit spec with
| Some a, Some b when a <> b -> printfn $"dotnet/fsharp changes: https://github.com/dotnet/fsharp/compare/%s{a}...%s{b}"
| _, Some b -> printfn $"dotnet/fsharp commit: https://github.com/dotnet/fsharp/commit/%s{b}"
| _ -> ()

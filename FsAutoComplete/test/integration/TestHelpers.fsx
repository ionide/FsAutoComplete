open System
open System.IO
open System.Diagnostics
open System.Text.RegularExpressions

#I "../../../packages/Newtonsoft.Json/lib/net45/"
#r "../../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json

type FsAutoCompleteWrapper() =

  let p = new System.Diagnostics.Process()

  do
    p.StartInfo.FileName <-
      IO.Path.Combine(__SOURCE_DIRECTORY__,
                      "../../bin/Debug/fsautocomplete.exe")
    p.StartInfo.RedirectStandardOutput <- true
    p.StartInfo.RedirectStandardError  <- true
    p.StartInfo.RedirectStandardInput  <- true
    p.StartInfo.UseShellExecute <- false
    p.StartInfo.EnvironmentVariables.Add("FSharpBinding_BlockingTimeout", "5000")
    p.StartInfo.EnvironmentVariables.Add("FSharpBinding_MaxTimeout", "10000")
    p.Start () |> ignore

  member x.project (s: string) : unit =
    fprintf p.StandardInput "project \"%s\"\n" s

  member x.parse (s: string) : unit =
    let text = if IO.File.Exists s then IO.File.ReadAllText(s) else ""
    fprintf p.StandardInput "parse \"%s\" sync\n%s\n<<EOF>>\n" s text

  member x.parseContent (filename: string) (content: string) : unit =
    fprintf p.StandardInput "parse \"%s\" sync\n%s\n<<EOF>>\n" filename content

  member x.completion (fn: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "completion \"%s\" %d %d\n" fn line col

  member x.methods (fn: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "methods \"%s\" %d %d\n" fn line col

  member x.completionFilter (fn: string) (line: int) (col: int) (filter: string) : unit =
    fprintf p.StandardInput "completion \"%s\" %d %d filter=%s\n" fn line col filter

  member x.tooltip (fn: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "tooltip \"%s\" %d %d\n" fn line col

  member x.finddeclaration (fn: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "finddecl \"%s\" %d %d\n" fn line col

  member x.symboluse (fn: string) (line: int) (col: int) : unit =
    fprintf p.StandardInput "symboluse \"%s\" %d %d\n" fn line col

  member x.declarations (fn: string) : unit =
    fprintf p.StandardInput "declarations \"%s\"\n" fn

  member x.send (s: string) : unit =
    fprintf p.StandardInput "%s" s

  member x.finalOutput () : string =
    let s = p.StandardOutput.ReadToEnd()
    let t = p.StandardError.ReadToEnd()
    p.WaitForExit()
    s + t

let formatJson json =
    try
      let parsedJson = JsonConvert.DeserializeObject(json)
      JsonConvert.SerializeObject(parsedJson, Formatting.Indented)
    with _ -> json

let writeNormalizedOutput (fn: string) (s: string) =
  let lines = s.TrimEnd().Split('\n')
  for i in [ 0 .. lines.Length - 1 ] do
    if Path.DirectorySeparatorChar = '/' then
      lines.[i] <- Regex.Replace(lines.[i],
                                 "/.*?FsAutoComplete/test/(.*?(\"|$))",
                                 "<absolute path removed>/test/$1")
      lines.[i] <- Regex.Replace(lines.[i],
                                 "\"/[^\"]*?/([^\"/]*?\.dll\")",
                                  "\"<absolute path removed>/$1")
    else
      if Path.GetExtension fn = ".json" then
        lines.[i] <- Regex.Replace(lines.[i].Replace(@"\\", "/"),
                                   "[a-zA-Z]:/.*?FsAutoComplete/test/(.*?(\"|$))",
                                   "<absolute path removed>/test/$1")
        lines.[i] <- Regex.Replace(lines.[i],
                                   "\"[a-zA-Z]:/[^\"]*?/([^\"/]*?\.dll\")",
                                   "\"<absolute path removed>/$1")
      else
        lines.[i] <- Regex.Replace(lines.[i].Replace('\\','/'),
                                   "[a-zA-Z]:/.*?FsAutoComplete/test/(.*?(\"|$))",
                                   "<absolute path removed>/test/$1")

    if Path.GetExtension fn = ".json" then
      lines.[i] <- formatJson lines.[i]

    lines.[i] <- lines.[i].Replace("\r", "")

  // Write manually to ensure \n line endings on all platforms
  using (new StreamWriter(fn))
  <| fun f ->
      for line in lines do
        f.Write(line)
        f.Write('\n')

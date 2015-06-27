// --------------------------------------------------------------------------------------
// (c) Tomas Petricek, http://tomasp.net/blog
// --------------------------------------------------------------------------------------
namespace FSharp.InteractiveAutocomplete

open System
open System.IO

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharp.CompilerBinding

open Newtonsoft.Json
open Newtonsoft.Json.Converters

module FsParser = Microsoft.FSharp.Compiler.Parser

/// The possible types of output
type OutputMode =
  | Json
  | Text

/// Represents information needed to call the F# IntelliSense service
/// (including project/script options, file name and source)
type internal RequestOptions(opts, file, src) =
  member x.Options : FSharpProjectOptions = opts
  member x.FileName : string = file
  member x.Source : string = src
  member x.WithSource(source) =
    RequestOptions(opts, file, source)

  override x.ToString() =
    sprintf "FileName: '%s'\nSource length: '%d'\nOptions: %s, %A, %A, %b, %b"
      x.FileName x.Source.Length x.Options.ProjectFileName x.Options.ProjectFileNames
      x.Options.OtherOptions x.Options.IsIncompleteTypeCheckEnvironment
      x.Options.UseScriptResolutionRules

type ResponseMsg<'T> =
  {
    Kind: string
    Data: 'T
  }

type Location =
  {
    File: string
    Line: int
    Column: int
  }

type CompletionResponse =
  {
    Name: string
    Glyph: string
    GlyphChar: string
  }

type ProjectResponse =
  {
    Project: string
    Files: List<string>
    Output: string
    References: List<string>
    Framework: string
  }

type OverloadParameter =
  {
    Name : string
    CanonicalTypeTextForSorting : string
    Display : string
    Description : string
  }
type Overload =
  {
    Tip : string
    TypeText : string
    Parameters : OverloadParameter list
    IsStaticArguments : bool
  }
type MethodResponse =
  {
    Name : string
    Overloads : Overload list
  }

type FSharpErrorSeverityConverter() =
  inherit JsonConverter()

  override x.CanConvert(t:System.Type) = t = typeof<FSharpErrorSeverity>

  override x.WriteJson(writer, value, serializer) =
    match value :?> FSharpErrorSeverity with
    | FSharpErrorSeverity.Error -> serializer.Serialize(writer, "Error")
    | FSharpErrorSeverity.Warning -> serializer.Serialize(writer, "Warning")

  override x.ReadJson(_reader, _t, _, _serializer) =
    raise (System.NotSupportedException())

  override x.CanRead = false
  override x.CanWrite = true

type RangeConverter() =
  inherit JsonConverter()

  override x.CanConvert(t:System.Type) = t = typeof<Range.range>

  override x.WriteJson(writer, value, serializer) =
    let range = value :?> Range.range
    serializer.Serialize(writer, (range.Start, range.End))

  override x.ReadJson(_reader, _t, _, _serializer) =
    raise (System.NotSupportedException())

  override x.CanRead = false
  override x.CanWrite = true

// --------------------------------------------------------------------------------------
// Utilities for parsing & processing command line input
// --------------------------------------------------------------------------------------

module internal CommandInput =
  open Parser

  let helpText = @"
    Supported commands
    ==================
    help
      - display this help message
    quit
      - quit the program
    declarations ""filename""
      - get information about top-level declarations in a file with location
    parse ""<filename>"" [sync]
      - trigger full background parse request; should be
        followed by content of a file (ended with <<EOF>>)
        Optional 'sync' is used to force the parse to occur
        synchronously for testing purposes. Not intended for
        use in production.
    completion ""<filename>"" <line> <col> [timeout] [filter=(StartsWith|Contains)]
      - trigger completion request for the specified location
        optionally filter in the specified manner
    helptext <candidate>
      - fetch type signature for specified completion candidate
        (from last completion request). Only use in JSON mode.
    tooltip ""<filename>"" <line> <col> [timeout]
      - get tool tip for the specified location
    finddecl ""<filename>"" <line> <col> [timeout]
      - find the point of declaration of the symbol at specified location
    methods ""<filename>"" <line> <col> [timeout]
      - find the method signatures at specified location
    project ""<filename>""
      - associates the current session with the specified project
    outputmode {json,text}
      - switches the output format. json offers richer data
        for some commands
    compilerlocation
      - prints the best guess for the location of fsc and fsi
        (or fsharpc and fsharpi on unix)
    "

  let outputText = @"
    Output format
    =============

    Messages are in one of the following three forms:

    1. INFO: text
       <<EOF>>

       A single line with a free text field. Returns information.

    2. ERROR: text
       <<EOF>>

       A single line with a free text field. An error has occurred.

    3. DATA: word
       text
       <<EOF>>

       Some data, where 'word' (in [a-z]+) indicates the type of data
       followed by some lines of free text, terminated by the special
       string <<EOF>>"

  // The types of commands that need position information
  type PosCommand =
    | Completion
    | Methods
    | ToolTip
    | FindDeclaration

  type ParseKind =
    | Normal
    | Synchronous

  // Command that can be entered on the command-line
  type Command =
    | PosCommand of PosCommand * string * int * int * int option * string option
    | HelpText of string
    | Declarations of string
    | Parse of string * ParseKind
    | Error of string
    | Project of string
    | OutputMode of OutputMode
    | CompilerLocation
    | Help
    | Quit

  /// Parse 'help' command
  let help = string "help" |> Parser.map (fun _ -> Help)

  /// Parse 'quit' command
  let quit = string "quit" |> Parser.map (fun _ -> Quit)

  /// Parse 'declarations' command
  let declarations = parser {
    let! _ = string "declarations "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.ofSeq
    let! _ = char '"'
    return Declarations(filename) }

  /// Parse 'outputmode' command
  let outputmode = parser {
    let! _ = string "outputmode "
    let! mode = (parser { let! _ = string "json"
                          return Json }) <|>
                (parser { let! _ = string "text"
                          return Text })
    return OutputMode mode }

  /// Parse 'project' command
  let project = parser {
    let! _ = string "project "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.ofSeq
    let! _ = char '"'
    return Project(filename) }

  /// Read multi-line input as a list of strings
  let rec readInput input =
    let str = Console.ReadLine()
    if str = "<<EOF>>" then List.rev input
    else readInput (str::input)

  // Parse 'parse "<filename>" [full]' command
  let parse = parser {
    let! _ = string "parse "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.ofSeq
    let! _ = char '"'
    let! _ = many (string " ")
    let! full = (parser { let! _ = string "sync"
                          return Synchronous }) <|>
                (parser { return Normal })
    return Parse (filename, full) }

  // Parse 'completion "<filename>" <line> <col> [timeout]' command
  let completionTipOrDecl = parser {
    let! f = (string "completion " |> Parser.map (fun _ -> Completion)) <|>
             (string "tooltip " |> Parser.map (fun _ -> ToolTip)) <|>
             (string "methods " |> Parser.map (fun _ -> Methods)) <|>
             (string "finddecl " |> Parser.map (fun _ -> FindDeclaration))
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.ofSeq
    let! _ = char '"'
    let! _ = many (string " ")
    let! line = some digit |> Parser.map (String.ofSeq >> int)
    let! _ = many (string " ")
    let! col = some digit |> Parser.map (String.ofSeq >> int)
    let! timeout =
      (parser { let! _ = some (string " ")
                return! some digit |> Parser.map (String.ofSeq >> int >> Some) }) <|>
      (parser { return None })
    let! filter =
      (parser { let! _ = many (string " ")
                let! _ = string "filter="
                let! b = (string "StartsWith" <|> string "Contains")
                         |> Parser.map String.ofSeq
                return Some b }) <|>
      (parser { return None })
    return PosCommand(f, filename, line, col, timeout, filter) }

  let helptext = parser {
      let! _ = string "helptext"
      let! _ = some (string " ")
      let! sym = many (sat (fun _ -> true)) |> Parser.map String.ofSeq
      return HelpText sym
    }

  let compilerlocation = parser {
    let! _ = string "compilerlocation"
    return CompilerLocation
    }

  // Parses always and returns default error message
  let error = parser { return Error("Unknown command or wrong arguments") }

  // Parse any of the supported commands
  let parseCommand =
    function
    | null -> Quit
    | input ->
      let reader = Parsing.createForwardStringReader input 0
      let cmds = compilerlocation <|> outputmode <|> helptext <|> help <|> declarations <|> parse <|> project <|> completionTipOrDecl <|> quit <|> error
      reader |> Parsing.getFirst cmds

module internal CompletionUtils =
  let map =
    [ 0x0000,  ("Class", "C")
      0x0003,  ("Enum", "E")
      0x00012, ("Struct", "S")
      0x00018, ("Struct", "S") (* value type *)
      0x0002,  ("Delegate", "D")
      0x0008,  ("Interface", "I")
      0x000e,  ("Module", "N") (* module *)
      0x000f,  ("Namespace", "N")
      0x000c,  ("Method", "M")
      0x000d,  ("Extension Method", "M") (* method2 ? *)
      0x00011, ("Property", "P")
      0x0005,  ("Event", "e")
      0x0007,  ("Field", "F") (* fieldblue ? *)
      0x0020,  ("Field", "F") (* fieldyellow ? *)
      0x0001,  ("Field", "F") (* const *)
      0x0004,  ("Property", "P") (* enummember *)
      0x0006,  ("Exception", "X") (* exception *)
      0x0009,  ("Text File Icon", "t") (* TextLine *)
      0x000a,  ("Regular File", "R") (* Script *)
      0x000b,  ("Script", "s") (* Script2 *)
      0x0010,  ("Tip of the day", "t") (* Formula *);
      0x00013, ("Class", "C") (* Template *)
      0x00014, ("Class", "C") (* Typedef *)
      0x00015, ("Type", "T") (* Type *)
      0x00016, ("Type", "T") (* Union *)
      0x00017, ("Field", "F") (* Variable *)
      0x00019, ("Class", "C") (* Intrinsic *)
      0x0001f, ("Other", "o") (* error *)
      0x00021, ("Other", "o") (* Misc1 *)
      0x0022,  ("Other", "o") (* Misc2 *)
      0x00023, ("Other", "o") (* Misc3 *) ] |> Map.ofSeq

  /// Translates icon code that we get from F# language service into a MonoDevelop icon
  let getIcon glyph =
    match map.TryFind (glyph / 6), map.TryFind (glyph % 6) with
    | Some(s), _ -> s // Is the second number good for anything?
    | _, _ -> ("", "")



// --------------------------------------------------------------------------------------
// Main application command-line loop
// --------------------------------------------------------------------------------------

/// Represents current state
type internal State =
  {
    Files : Map<string,VolatileFile> //filename -> lines * touch date
    Projects : Map<string, FSharpProjectFileInfo>
    OutputMode : OutputMode
    HelpText : Map<String, FSharpToolTipText>
  }

/// Contains main loop of the application
module internal Main =
  open CommandInput

  type internal PrintingAgent() =
   let agent = MailboxProcessor.Start(fun agent ->
     let rec loop () = async {
         let! (msg: Choice<string,AsyncReplyChannel<unit>>) = agent.Receive()
         match msg with
         | Choice1Of2 (s: string) -> Console.WriteLine s; return! loop ()
         | Choice2Of2 ch -> ch.Reply ()
       }
     loop ()
     )

   member x.WriteLine(s) = agent.Post (Choice1Of2 s)

   member x.Quit() = agent.PostAndReply(fun ch -> Choice2Of2 ch)

  let initialState = { Files = Map.empty; Projects = Map.empty; OutputMode = Text; HelpText = Map.empty }

  let printAgent = new PrintingAgent()

  let jsonConverters =
    [|
     new FSharpErrorSeverityConverter() :> JsonConverter;
     new RangeConverter() :> JsonConverter
    |]

  let prAsJson o = printAgent.WriteLine (JsonConvert.SerializeObject(o, jsonConverters))
  let printMsg state ty s =
      match state.OutputMode with
      | Text -> printAgent.WriteLine (sprintf "%s: %s\n<<EOF>>" ty s)
      | Json -> prAsJson { Kind = ty; Data = s }

  // Main agent that handles IntelliSense requests
  let agent = new FSharp.CompilerBinding.LanguageService(fun _ -> ())

  let mutable currentFiles = Map.empty
  let originalFs = Microsoft.FSharp.Compiler.AbstractIL.Internal.Library.Shim.FileSystem
  let fs = new FileSystem(originalFs, fun () -> currentFiles)
  Microsoft.FSharp.Compiler.AbstractIL.Internal.Library.Shim.FileSystem <- fs

  let rec main (state:State) : int =
    currentFiles <- state.Files
    let printMsg = printMsg state

    let parsed file =
      let ok = Map.containsKey file state.Files
      if not ok then printMsg "ERROR" (sprintf "File '%s' not parsed" file)
      ok

    /// Is the specified position consistent with internal state of file?
    //  Note that both emacs and FCS use 1-based line indexing
    //  while emacs uses 0-based column indexing and FCS is 1-based
    let posok file line col =
      let lines = state.Files.[file].Lines
      let ok = line <= lines.Length && line >= 1 &&
               col <= lines.[line - 1].Length + 1 && col >= 1
      if not ok then printMsg "ERROR" "Position is out of range"
      ok

    let getoptions file state =
      let text = String.concat "\n" state.Files.[file].Lines
      let project = Map.tryFind file state.Projects
      let projFile, args =
          match project with
          | None -> file, [|file|]
          | Some p -> p.Directory + "/Project.fsproj", Array.ofList p.Options
      text, projFile, args

    // Debug.print "main state is:\nproject: %b\nfiles: %A\nmode: %A"
    //             (Option.isSome state.Project)
    //             (Map.fold (fun ks k _ -> k::ks) [] state.Files)
    //             state.OutputMode
    match parseCommand(Console.ReadLine()) with

    | OutputMode m -> main { state with OutputMode = m }

    | Parse(file,kind) ->
        // Trigger parse request for a particular file
        let lines = readInput [] |> Array.ofList
        let file = Path.GetFullPath file
        let state' =  { state with Files = state.Files |> Map.add file
                                                        { Lines = lines
                                                          Touched = DateTime.Now } }
        let text, projFile, args = getoptions file state'

        let task =
          async {
            let! results = agent.ParseAndCheckFileInProject(projFile, file, text, [||], args, true)
            match results.GetErrors() with
            | None -> ()
            | Some errs ->
              match state.OutputMode with
              | Text ->
                let sb = new System.Text.StringBuilder()
                sb.AppendLine("DATA: errors") |> ignore
                for e in errs do
                  sb.AppendLine(sprintf "[%d:%d-%d:%d] %s %s" e.StartLineAlternate e.StartColumn e.EndLineAlternate e.EndColumn
                                  (if e.Severity = FSharpErrorSeverity.Error then "ERROR" else "WARNING") e.Message)
                  |> ignore
                sb.Append("<<EOF>>") |> ignore
                printAgent.WriteLine(sb.ToString())

              | Json -> prAsJson { Kind = "errors"; Data = errs }
          }

        match kind with
        | Synchronous -> printMsg "INFO" "Synchronous parsing started"
                         Async.RunSynchronously task
        | Normal -> printMsg "INFO" "Background parsing started"
                    Async.StartImmediate task


        main state'

    | Project file ->
        // Load project file and store in state
        let file = Path.GetFullPath file
        if File.Exists file then
          try
            let p = SourceCodeServices.FSharpProjectFileInfo.Parse(file)
            let files =
              [ for f in p.CompileFiles do
                  yield IO.Path.Combine(p.Directory, f) ]
            // TODO: Handle these options more gracefully
            let targetFilename = match p.OutputFile with Some p -> p | None -> "Unknown"
            let framework = match p.FrameworkVersion with Some p -> p | None -> "Unknown"
            match state.OutputMode with
            | Text -> printAgent.WriteLine(sprintf "DATA: project\n%s\n<<EOF>>" (String.concat "\n" files))
            | Json -> prAsJson { Kind = "project"
                                 Data = { Project = file
                                          Files = files
                                          Output = targetFilename
                                          References = List.sortBy Path.GetFileName p.References
                                          Framework = framework } }
            let projects =
              files
              |> List.fold (fun s f -> Map.add f p s) state.Projects
            main { state with Projects = projects }
          with e ->
            printMsg "ERROR" (sprintf "Project file '%s' is invalid: '%s'" file e.Message)
            main state
        else
          printMsg "ERROR" (sprintf "File '%s' does not exist" file)
          main state

    | Declarations file ->
        let file = Path.GetFullPath file
        if parsed file then
          let text, projFile, args = getoptions file state
          let parseResult = agent.ParseFileInProject(projFile, file, text, args) |> Async.RunSynchronously
          let decls = parseResult.GetNavigationItems().Declarations
          match state.OutputMode with
          | Text ->
              let declstrings =
                [ for tld in decls do
                    let m = tld.Declaration.Range
                    let (s1, e1), (s2, e2) =  ((m.StartColumn, m.StartLine), (m.EndColumn, m.EndLine))
                    yield sprintf "[%d:%d-%d:%d] %s" e1 s1 e2 s2 tld.Declaration.Name
                    for d in tld.Nested do
                      let m = d.Range
                      let (s1, e1), (s2, e2) = ((m.StartColumn, m.StartLine), (m.EndColumn, m.EndLine))
                      yield sprintf "  - [%d:%d-%d:%d] %s" e1 s1 e2 s2 d.Name ]
              printAgent.WriteLine(sprintf "DATA: declarations\n%s\n<<EOF>>" (String.concat "\n" declstrings))
          | Json -> prAsJson { Kind = "declarations"; Data = decls }
        main state

    | HelpText sym ->

        match Map.tryFind sym state.HelpText with
        | None -> ()
        | Some d ->

          let tip = TipFormatter.formatTip d
          let helptext = Map.add sym tip Map.empty
          prAsJson { Kind = "helptext"; Data = helptext }

        main state

    | PosCommand(cmd, file, line, col, timeout, filter) ->
        let file = Path.GetFullPath file
        if parsed file && posok file line col then
          let text, projFile, args = getoptions file state
          let lineStr = state.Files.[file].Lines.[line - 1]
          // TODO: Deny recent typecheck results under some circumstances (after bracketed expr..)
          let timeout = match timeout with Some x -> x | _ -> 20000
          let tyResOpt = agent.GetTypedParseResultWithTimeout(projFile, file, text, [||], args, AllowStaleResults.MatchingFileName, timeout)
                         |> Async.RunSynchronously
          match tyResOpt with
          | None -> printMsg "ERROR" "Timeout when fetching typed parse result"; main state
          | Some tyRes ->

          match cmd with
          | Completion ->

              match tyRes.GetDeclarations(line, col, lineStr) with
              | Some (decls, residue) ->
                  let decls =
                    match filter with
                    | Some "StartsWith" ->  [| for d in decls.Items do if d.Name.StartsWith residue then yield d |]
                    | Some "Contains" -> [| for d in decls.Items do if d.Name.Contains residue then yield d |]
                    | _ -> decls.Items

                  match state.OutputMode with
                  | Text ->
                      printAgent.WriteLine "DATA: completion"
                      for d in decls do printAgent.WriteLine(d.Name)
                      printAgent.WriteLine "<<EOF>>"
                      main state
                  | Json ->

                      let ds = Array.sortBy (fun (d: FSharpDeclarationListItem) -> d.Name) decls
                      match Array.tryFind (fun (d: FSharpDeclarationListItem) -> d.Name.StartsWith residue) ds with
                      | None -> ()
                      | Some d -> let tip = TipFormatter.formatTip d.DescriptionText
                                  let helptext = Map.add d.Name tip Map.empty
                                  prAsJson { Kind = "helptext"; Data = helptext }

                      prAsJson { Kind = "completion"
                                 Data = [ for d in decls do
                                            let (glyph, glyphChar) = CompletionUtils.getIcon d.Glyph
                                            yield { Name = d.Name; Glyph = glyph; GlyphChar = glyphChar } ] }

                      let helptext =
                        Seq.fold (fun m (d: FSharpDeclarationListItem) -> Map.add d.Name d.DescriptionText m) Map.empty decls

                      main { state with HelpText = helptext }
              | None ->
                  printMsg "ERROR" "Could not get type information"
                  main state

          | ToolTip ->

              let tipopt = tyRes.GetToolTip(line, col, lineStr)
                           |> Async.RunSynchronously

              match tipopt with
              | None -> printMsg "INFO" "No tooltip information"
              | Some (tip,_) ->
                match tip with
                | FSharpToolTipText(elems) when elems |> List.forall (function
                  FSharpToolTipElement.None -> true | _ -> false) ->
                  printMsg "INFO" "No tooltip information"
                | _ ->
                    match state.OutputMode with
                    | Text ->
                      printAgent.WriteLine("DATA: tooltip")
                      printAgent.WriteLine(TipFormatter.formatTip tip)
                      printAgent.WriteLine("<<EOF>>")
                    | Json -> prAsJson { Kind = "tooltip"; Data = TipFormatter.formatTip tip }

              main state

          | FindDeclaration ->
            let declarations = tyRes.GetDeclarationLocation(line,col,lineStr)
                               |> Async.RunSynchronously
            match declarations with
            | FSharpFindDeclResult.DeclNotFound _ -> printMsg "ERROR" "Could not find declaration"
            | FSharpFindDeclResult.DeclFound range ->

              match state.OutputMode with
              | Text -> printAgent.WriteLine(sprintf "DATA: finddecl\n%s:%d:%d\n<<EOF>>" range.FileName range.StartLine range.StartColumn)
              | Json ->
                  let data = { Line = range.StartLine; Column = range.StartColumn; File = range.FileName }
                  prAsJson { Kind = "finddecl"; Data = data }

            main state


          | Methods ->
            // Find the starting point, ideally right after the first '('
            let lineCutoff = line - 3
            let line, col =
              let rec prevPos (line,col) =
                match line, col with
                | 1, 1
                | _ when line < lineCutoff -> 1, 1
                | _, 1 ->
                   let prevLine = state.Files.[file].Lines.[line - 2]
                   if prevLine.Length = 0 then prevPos(line-1, 1)
                   else line - 1, prevLine.Length
                | _    -> line, col - 1

              let rec loop depth (line, col) =
                if (line,col) <= (1,1) then (line, col) else
                let ch = state.Files.[file].Lines.[line - 1].[col - 1]
                if (ch = '(' || ch = '{' || ch = '[') && depth > 0 then loop (depth - 1) (prevPos (line,col))
                elif ch = ')' || ch = '}' || ch = ']' then loop (depth + 1) (prevPos (line,col))
                elif ch = '(' || ch = '<' then line,col
                else loop depth (prevPos (line,col))
              match loop 0 (prevPos(line,col)) with
              | 1, 1 -> line,col
              | newPos -> newPos

            let meth = tyRes.GetMethods(line, col, state.Files.[file].Lines.[line - 1])
                       |> Async.RunSynchronously
            match meth with
            | Some (name,overloads) when overloads.Length > 0 ->
              match state.OutputMode with
              | Text ->
                  printMsg "ERROR" "methods not supported in text mode"
              | Json ->
                  prAsJson
                   { Kind = "method"
                     Data = { Name = name
                              Overloads =
                               [ for o in overloads do
                                  let tip = TipFormatter.formatTip o.Description
                                  yield {
                                    Tip = tip
                                    TypeText = o.TypeText
                                    Parameters =
                                      [ for p in o.Parameters do
                                         yield {
                                           Name = p.ParameterName
                                           CanonicalTypeTextForSorting = p.CanonicalTypeTextForSorting
                                           Display = p.Display
                                           Description = p.Description
                                         }
                                    ]
                                    IsStaticArguments = o.IsStaticArguments
                                  }

                               ] } }
            | _ -> printMsg "ERROR" "Could not find method"

            main state

        else
          main state

    | CompilerLocation ->
        let locopt = FSharpEnvironment.BinFolderOfDefaultFSharpCompiler None
        match locopt with
        | None -> printMsg "ERROR" "Could not find compiler"; main state
        | Some loc ->

        match state.OutputMode with
        | Text -> printfn "DATA: compilerlocation\n%s\n<<EOF>>" loc
        | Json -> prAsJson { Kind = "compilerlocation"; Data = loc }

        main state

    | Help ->
        match state.OutputMode with
        | Text -> printAgent.WriteLine(helpText)
        | Json -> prAsJson { Kind = "INFO"; Data = helptext }
        main state

    | Error(msg) ->
        printMsg "ERROR" msg
        main state

    | Quit ->
        printAgent.Quit()
        (!Debug.output).Close ()
        0

  [<EntryPoint>]
  let entry args =
    // System.Diagnostics.Debug.Listeners.Add(
    //   new System.Diagnostics.TextWriterTraceListener(Console.Out))
    // |> ignore
    let extra = Options.p.Parse args
    if extra.Count <> 0 then
      printfn "Unrecognised arguments: %s" (String.concat "," extra)
      1
    else
      try
        main initialState
      finally
        (!Debug.output).Close ()

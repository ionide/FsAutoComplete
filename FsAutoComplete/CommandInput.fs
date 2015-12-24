namespace FsAutoComplete

open Parser
open System
open Types
open Fantomas.FormatConfig

// The types of commands that need position information
type PosCommand =
  | Completion
  | Methods
  | SymbolUse
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
  | Parse of string * ParseKind * string[]
  | Error of string
  | Lint of string
  | Project of string * DateTime * bool
  | Colorization of bool
  | Format of fileName: string * config: Fantomas.FormatConfig.FormatConfig
  | FormatSelection of fileName : string * range : Range * config : Fantomas.FormatConfig.FormatConfig
  | CompilerLocation
  | Quit

module CommandInput =
  /// Parse 'quit' command
  let quit = string "quit" |> Parser.map (fun _ -> Quit)
  
  let quotedfilename = parser {
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.ofSeq
    let! _ = char '"'
    return filename
  }

  let bool =   parser { let! _ = string "true"
                      return true } <|>
               parser { let! _ = string "false"
                        return false }

  /// Parse 'colorizations' command
  let colorizations = parser {
      let! _ = string "colorizations "
      let! b = bool
      return Colorization b
    }

  /// Parse 'declarations' command
  let declarations = parser {
    let! _ = string "declarations "
    let! filename = quotedfilename 
    return Declarations(filename) 
  }

  /// Parse 'project' command
  let project = parser {
    let! _ = string "project "
    let! filename = quotedfilename
    let! verbose =
      (parser { let! _ = some (string " verbose")
                return true }) <|>
      (parser { return false })
    return Project(filename,DateTime.Now,verbose) }

  /// Parse 'lint' command
  let lint = parser {
    let! _ = string "lint "
    let! filename = quotedfilename
    return Lint(filename) }
  
  let digits = (some (sat Char.IsDigit)) |> Parser.map String.ofSeq |> Parser.map Int32.Parse

  // range will look like 25:12-500:100 for line 25, col 12 through line 500 col 100
  let range = parser {
    let _ = string "range "
    let! startline = digits
    let _ = string ":"
    let! startcol = digits
    let _ = string "-"
    let! endline = digits
    let _ = string ":"
    let! endcol = digits
    return {
    startLine = startline
    startCol = startcol
    endLine = endline
    endCol = endcol
    }
  }

  let file = parser {
    let _ = string "file "
    return! quotedfilename
  }

  let config = 
    let inputConfig = 
      let pprop name vparser func = parser {
        let! _ = string (sprintf "%s " name)
        let! value = vparser
        return fun config -> func config value
      }
    
      let indentSpace = pprop "spaceindent" digits (fun config value -> {config with IndentSpaceNum = value})
      let pageWidth = pprop "pagewidth" digits (fun config value -> { config with PageWidth = value })
      let semiAtEnd = pprop "endsemicolon" bool (fun config value -> { config with SemicolonAtEndOfLine = value })
      let spaceBeforeArg = pprop "spacebeforearg" bool (fun config value -> { config with SpaceBeforeArgument = value })
      let spaceBeforeColon = pprop "spacebeforecolon" bool (fun config value -> { config with SpaceBeforeColon = value })
      let spaceAfterComma = pprop "spaceaftercomma" bool (fun config value -> {config with SpaceAfterComma = value})
      let spaceAfterSemi = pprop "spaceaftersemi" bool (fun config value -> {config with SpaceAfterSemicolon = value})
      let indentTryWith = pprop "indenttrywith" bool (fun config value -> { config with IndentOnTryWith = value })
      let reorderOpens = pprop "reorderOpens" bool (fun config value -> {config with ReorderOpenDeclaration = value})
      let spaceDelims = pprop "surrounddelims" bool (fun config value -> {config with SpaceAroundDelimiter = value})
      let strict = pprop "strict" bool (fun config value -> { config with StrictMode = value })
      
      let opts = (indentSpace <|> pageWidth <|> semiAtEnd <|> spaceBeforeArg <|> spaceBeforeColon <|> spaceAfterComma <|> spaceAfterSemi <|> indentTryWith <|> reorderOpens <|> spaceDelims <|> strict) |> sep (string " ")

      parser {
        let! _ = string "config "
        let! mods = opts
        // need to collect the various flags and apply them to default config
        return (mods |> List.fold (fun c o -> o c) FormatConfig.Default)
      }
      
    let defaultConfig = parser { return FormatConfig.Default }
    inputConfig <|> defaultConfig

  let format = parser {
    let! _ = string "format "
    let! fileName = file
    let! _ = string " "
    let! config = config
    return Format(fileName, config)
  }

  let formatSelection = parser {
    let! _ = string "format "
    let! file = file
    let! _ = string " "
    let! range = range
    let! _ = string " "
    let! config = config
    return FormatSelection(file, range, config)
  } 

  /// Read multi-line input as a list of strings
  let rec readInput input =
    let str = Console.ReadLine()
    if str = "<<EOF>>" then List.rev input
    else readInput (str::input)

  // Parse 'parse "<filename>" [sync]' command
  let parse =
    parser {
      let! _ = string "parse "
      let! filename = quotedfilename
      let! _ = many (string " ")
      let! full = (parser { let! _ = string "sync"
                            return Synchronous }) <|>
                  (parser { return Normal })
      let lines = [||]
      return Parse (filename, full, lines) }

  // Parse 'completion "<filename>" <line> <col> [timeout]' command
  let completionTipOrDecl = parser {
    let! f = (string "completion " |> Parser.map (fun _ -> Completion)) <|>
             (string "symboluse " |> Parser.map (fun _ -> SymbolUse)) <|>
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
      let cmds = compilerlocation <|> helptext <|> declarations <|> lint <|> format <|> formatSelection <|> parse <|> project <|> completionTipOrDecl <|> quit <|> colorizations <|> error
      let cmd = reader |> Parsing.getFirst cmds
      match cmd with
      | Parse (filename,kind,_) ->
          let lines = readInput [] |> Array.ofList
          Parse (filename, kind, lines)
      | _ -> cmd

module Tests =
    open CommandInput

    let parseWithSome = apply config "config spaceindent 5" |> List.head |> (fun c -> c.IndentSpaceNum = 5)

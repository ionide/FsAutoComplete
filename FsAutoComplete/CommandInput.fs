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
  | Format of Fantomas.FormatConfig.FormatConfig * FormatData
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

  let format = 
      let file = parser {
          let _ = string "file "
          let! file = quotedfilename
          return Types.File(file)
      }

      let digit = (some (sat Char.IsDigit)) |> Parser.map String.ofSeq |> Parser.map Int32.Parse
      
      // range will look like 25:12-500:100 for line 25, col 12 through line 500 col 100
      let selection = parser {
        let _ = string "range "
        let! startline = digit
        let _ = string ":"
        let! startcol = digit
        let _ = string "-"
        let! endline = digit
        let _ = string ":"
        let! endcol = digit
        return {
            startLine = startline
            startCol = startcol
            endLine = endline
            endCol = endcol
        }
      }
    
      let selected = parser {
        let! _ = string "file "
        let! file = quotedfilename
        let! _ = string " "
        let! range = selection
        return Types.FileSelection(file, range)
      } 

      let inputConfig = 
        let pprop name func = parser {
            let! _ = string name
            let! value = bool
            return fun config -> func config value
        }
        let indentTryWith = pprop "indentOnTryWith" (fun config value-> {config with IndentOnTryWith = value})

        parser {
            let! _ = string "config "
            // need to collect the various flags and apply them to default config
            return { FormatConfig.Default with IndentOnTryWith = true}
        }
      
      let defaultConfig = parser { return FormatConfig.Default }

      parser {
          let! _ = string "format "
          let! config = inputConfig <|> defaultConfig
          let! formatdata = selected <|> file
          return Format(config, formatdata)
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
      let cmds = compilerlocation <|> helptext <|> declarations <|> lint <|> format <|> parse <|> project <|> completionTipOrDecl <|> quit <|> colorizations <|> error
      let cmd = reader |> Parsing.getFirst cmds
      match cmd with
      | Parse (filename,kind,_) ->
          let lines = readInput [] |> Array.ofList
          Parse (filename, kind, lines)
      | _ -> cmd

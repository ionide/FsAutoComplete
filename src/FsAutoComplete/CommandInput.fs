namespace FsAutoComplete

open Parser
open System
open FSharp.Compiler.Range

// The types of commands that need position information
type PosCommand =
  | Completion
  | Methods
  | SymbolUse
  | SymbolUseProject
  | SymbolImplementationProject
  | ToolTip
  | TypeSig
  | FindDeclaration
  | FindTypeDeclaration
  | SignatureData

type ParseKind =
  | Normal
  | Synchronous

// Command that can be entered on the command-line
type Command =
  | PosCommand of PosCommand * string * string * pos * int option * string option
  | HelpText of string
  | Declarations of string
  | Parse of string * ParseKind * string[]
  | Error of string
  | Lint of string
  | UnusedDeclarations of string
  | SimplifiedNames of string
  | UnusedOpens of string
  | Project of string * bool
  | Colorization of bool
  | CompilerLocation
  | WorkspacePeek of string * int * string[]
  | WorkspaceLoad of string[]
  | RegisterAnalyzer of string
  | Started
  | Quit
  | Fsdn of string
  | DotnetNewList of string
  | DotnetNewGetDetails of string
  | DotnetNewCreateCli of string * (string*obj) list

module CommandInput =
  /// Parse 'quit' command
  let quit = string "quit" |> Parser.map (fun _ -> Quit)

  /// Parse 'colorizations' command
  let colorizations = parser {
      let! _ = string "colorizations "
      let! b = parser { let! _ = string "true"
                        return true } <|>
               parser { let! _ = string "false"
                        return false }
      return Colorization b
    }

  /// Parse 'declarations' command
  let declarations = parser {
    let! _ = string "declarations "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
    let! _ = char '"'
    return Declarations(filename) }

  /// Parse 'project' command
  let project = parser {
    let! _ = string "project "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
    let! _ = char '"'
    let! verbose =
      (parser { let! _ = some (string " verbose")
                return true }) <|>
      (parser { return false })
    return Project(filename, verbose) }

  /// Parse 'lint' command
  let lint = parser {
      let! _ = string "lint "
      let! _ = char '"'
      let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      return Lint(filename) }

  let registerAnalyzer = parser {
    let! _ = string "analyzer "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
    let! _ = char '"'
    return RegisterAnalyzer(filename) }

  let unusedDeclarations = parser {
      let! _ = string "unusedDeclarations "
      let! _ = char '"'
      let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      return UnusedDeclarations(filename) }

  let simplifiedNames = parser {
    let! _ = string "simplifiedNames "
    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
    let! _ = char '"'
    return SimplifiedNames(filename) }

  let unusedOpens = parser {
      let! _ = string "unusedOpens "
      let! _ = char '"'
      let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      return UnusedOpens(filename) }

  /// Read multi-line input as a list of strings
  let rec readInput input =
    match Console.ReadLine() with
    | null | "<<EOF>>" -> List.rev input
    | str -> readInput (str::input)

  // Parse 'parse "<filename>" [sync]' command
  let parse =
    parser {
      let! _ = string "parse "
      let! _ = char '"'
      let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      let! _ = many (string " ")
      let! full = (parser { let! _ = string "sync"
                            return Synchronous }) <|>
                  (parser { return Normal })
      let lines = [||]
      return Parse (filename, full, lines) }

  let escapedQuote = parser {
    let! _ = char '\\'
    let! _ = char '"'
    return '"'
  }

  // Parse 'workspacepeek "<directory>" <deepLevel>' command
  let workspacePeek =
    parser {
      let! _ = string "workspacepeek "
      let! _ = char '"'
      let! dir = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      let! _ = many (string " ")
      let! deep = some digit |> Parser.map (String.OfSeq >> int)
      let excludeDir = [| |]
      return WorkspacePeek (dir, deep, excludeDir) }

  // Parse 'workspaceload "<filename>" "<filename>" .. "<filename>"' command
  let workspaceLoad =
    parser {
      let! _ = string "workspaceload "
      let parseFilename =
        parser {
          let! _ = char '"'
          let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
          let! _ = char '"'
          let! _ = many (string " ")
          return filename
        }
      let! files = many parseFilename
      return WorkspaceLoad (files |> Array.ofList) }

  let fsdn = parser {
      let! _ = string "fsdn "
      let! _ = char '"'
      let! querystr = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      return (Fsdn querystr)
      }

  let dotnetnewlist = parser {
      let! _ = string "dotnetnewlist "
      let! _ = char '"'
      let! filterstr = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      return (DotnetNewList filterstr)
  }
      
  let dotnetnewgetDetails = parser {
      let! _ = string "dotnetnewgetDetails "
      let! _ = char '"'
      let! filterstr = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      return (DotnetNewGetDetails filterstr)
  }

  let dotnetnewCreateCli = parser {
      let! _ = string "dotnetnewCreateCli "
      let! _ = char '"'
      let! templateShortName = some (sat ((<>) '"')) |> Parser.map String.OfSeq
      let! _ = char '"'
      let! parameterList = [| |]
      return DotnetNewCreateCli (templateShortName, parameterList)
  }

  // Parse 'completion "<filename>" "<linestr>" <line> <col> [timeout]' command
  let completionTipOrDecl = parser {
    let! f = (string "completion " |> Parser.map (fun _ -> Completion)) <|>
             (string "symboluse " |> Parser.map (fun _ -> SymbolUse)) <|>
             (string "symboluseproject " |> Parser.map (fun _ -> SymbolUseProject)) <|>
             (string "symbolimplementation " |> Parser.map (fun _ -> SymbolImplementationProject)) <|>
             (string "tooltip " |> Parser.map (fun _ -> ToolTip)) <|>
             (string "typesig " |> Parser.map (fun _ -> TypeSig)) <|>
             (string "methods " |> Parser.map (fun _ -> Methods)) <|>
             (string "finddecl " |> Parser.map (fun _ -> FindDeclaration)) <|>
             (string "findtypedecl " |> Parser.map (fun _ -> FindTypeDeclaration)) <|>
             (string "sigdata " |> Parser.map (fun _ -> SignatureData))

    let! _ = char '"'
    let! filename = some (sat ((<>) '"')) |> Parser.map String.OfSeq
    let! _ = char '"'
    let! _ = many (string " ")
    let! _ = char '"'
    let! lineStr = some (sat ((<>) '"') <|> escapedQuote) |> Parser.map String.OfSeq
    let! _ = char '"'
    let! _ = many (string " ")
    let! line = some digit |> Parser.map (String.OfSeq >> int)
    let! _ = many (string " ")
    let! col = some digit |> Parser.map (String.OfSeq >> int)
    let! timeout =
      (parser { let! _ = some (string " ")
                return! some digit |> Parser.map (String.OfSeq >> int >> Some) }) <|>
      (parser { return None })
    let! filter =
      (parser { let! _ = many (string " ")
                let! _ = string "filter="
                let! b = (string "StartsWith" <|> string "Contains")
                         |> Parser.map String.OfSeq
                return Some b }) <|>
      (parser { return None })
    return PosCommand(f, filename, lineStr, mkPos line col, timeout, filter) }

  let helptext = parser {
      let! _ = string "helptext"
      let! _ = some (string " ")
      let! sym = many (sat (fun _ -> true)) |> Parser.map String.OfSeq
      return HelpText sym
    }

  let compilerlocation = parser {
    let! _ = string "compilerlocation"
    return CompilerLocation
    }

  // Parses always and returns default error message
  let error = parser { return Error("Unknown command or wrong arguments") }

  // Parse any of the supported commands
  let parseCommand inputString =
    match inputString with
    | null -> Quit
    | input ->
      let reader = Parser.createForwardStringReader input 0
      let cmds = compilerlocation <|> helptext <|> declarations <|> lint <|> registerAnalyzer <|> unusedDeclarations <|> simplifiedNames <|> unusedOpens <|> parse <|> project <|> completionTipOrDecl <|> quit <|> colorizations <|> workspacePeek <|> workspaceLoad <|> fsdn <|> dotnetnewlist <|> dotnetnewgetDetails <|> dotnetnewCreateCli <|> error
      let cmd = reader |> Parser.getFirst cmds
      match cmd with
      | Parse (filename,kind,_) ->
          let lines = readInput [] |> Array.ofList
          Parse (filename, kind, lines)
      | _ -> cmd

#r "../../src/FsAutoComplete/bin/Debug/net6.0/fsautocomplete.dll"
#r "../../src/FsAutoComplete/bin/Debug/net6.0/LanguageServerProtocol.dll"

open FsAutoComplete
open FsAutoComplete.LspHelpers
open LanguageServerProtocol.Types

let decodeHighlighting (data: uint32 []) =
  let zeroLine = [| 0u; 0u; 0u; 0u; 0u |]

  let lines = Array.append [| zeroLine |] (Array.chunkBySize 5 data)

  let structures =
    let mutable lastLine = 0
    let mutable lastCol = 0

    lines
    |> Array.map (fun current ->
      let startLine = lastLine + int current.[0]

      let startCol =
        if current.[0] = 0u then
          lastCol + int current.[1]
        else
          int current.[1]

      let endLine = int startLine // assuming no multiline for now
      let endCol = startCol + int current.[2]
      lastLine <- startLine
      lastCol <- startCol
      let tokenType = enum<ClassificationUtils.SemanticTokenTypes> (int current.[3])
      let tokenMods = enum<ClassificationUtils.SemanticTokenModifier> (int current.[4])

      let range =
        { Start =
            { Line = startLine
              Character = startCol }
          End = { Line = endLine; Character = endCol } }

      range, tokenType, tokenMods)

  structures

/// add your own data array (likely taken from an LSP dump here) to dump out the decoded token stream
let data = [||] |> Array.map uint32

decodeHighlighting data

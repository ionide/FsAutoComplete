
[<RequireQualifiedAccess>]
module FsAutoComplete.AstFormat

module ParseTree =
    open FSharp.Compiler.SyntaxTree

    let printfFormat (tree: ParsedInput) = sprintf "%A" tree

module TypedTree =
    open FSharp.Compiler.SourceCodeServices

    let printfFormat (tast: FSharpImplementationFileContents) = sprintf "%A" tast.Declarations

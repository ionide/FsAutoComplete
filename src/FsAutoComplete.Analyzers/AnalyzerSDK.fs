module AnalyzerSDK

open System
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

/// Marks an analyzer for scanning
[<AttributeUsage(AttributeTargets.Method ||| AttributeTargets.Property ||| AttributeTargets.Field)>]
type AnalyzerAttribute() = inherit Attribute()

type Context =
    { FileName: string
      Content: string[]
      ParseTree: ParsedInput
      AbstractSyntaxTree: FSharpImplementationFileContents
      Symbols: FSharpEntity list }

type Fix =
    { FromRange : Range
      FromText : string
      ToText : string }

type Severity =
    | Info
    | Warning
    | Error

type Message =
    { Type: string
      Message: string
      Code: string
      Severity: Severity
      Fixes: Fix list }

type Analyzer = Context -> Message list
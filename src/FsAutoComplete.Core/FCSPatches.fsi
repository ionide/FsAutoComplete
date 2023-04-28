/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions
module FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FsAutoComplete.UntypedAstUtils
open FSharp.Compiler.CodeAnalysis

type FSharpParseFileResults with

    member IsPositionContainedInACurriedParameter: pos: pos -> bool
    member TryRangeOfParenEnclosingOpEqualsGreaterUsage: opGreaterEqualPos: pos -> (range * range * range) option
    member TryRangeOfRefCellDereferenceContainingPos: expressionPos: pos -> range option
    member TryRangeOfRecordExpressionContainingPos: pos: pos -> range option
    member TryRangeOfExprInYieldOrReturn: pos: pos -> range option
    /// Attempts to find an Ident of a pipeline containing the given position, and the number of args already applied in that pipeline.
    /// For example, '[1..10] |> List.map ' would give back the ident of '|>' and 1, because it applied 1 arg (the list) to 'List.map'.
    member TryIdentOfPipelineContainingPosAndNumArgsApplied: pos: pos -> (Ident * int) option
    /// Determines if the given position is inside a function or method application.
    member IsPosContainedInApplicationPatched: pos: pos -> bool
    /// Attempts to find the range of a function or method that is being applied. Also accounts for functions in pipelines.
    member TryRangeOfFunctionOrMethodBeingAppliedPatched: pos: pos -> Range option
    /// Gets the ranges of all arguments, if they can be found, for a function application at the given position.
    member GetAllArgumentsForFunctionApplicationAtPostion: pos: pos -> range list option
    member TryRangeOfExpressionBeingDereferencedContainingPos: expressionPos: pos -> range option
    /// Attempts to find the range of the string interpolation that contains a given position.
    member TryRangeOfStringInterpolationContainingPos: pos: pos -> range option

module SyntaxTreeOps =
    open FSharp.Compiler.Syntax
    val synExprContainsError: inpExpr: SynExpr -> bool

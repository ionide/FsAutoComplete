/// Original code from VisualFSharpPowerTools project: https://github.com/fsprojects/VisualFSharpPowerTools/blob/master/src/FSharp.Editing/CodeGeneration/UnionPatternMatchCaseGenerator.fs
module FsAutoComplete.UnionPatternMatchCaseGenerator

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Symbols

[<NoEquality; NoComparison>]
type PatternMatchExpr =
  {
    /// Range of 'match x with' or 'function'
    MatchWithOrFunctionRange: Range
    /// The whole pattern match expression
    Expr: SynExpr
    Clauses: SynMatchClause list
  }

[<NoComparison>]
type UnionMatchCasesInsertionParams =
  { InsertionPos: Position
    IndentColumn: int }

val shouldGenerateUnionPatternMatchCases: patMatchExpr: PatternMatchExpr -> entity: FSharpEntity -> bool

val tryFindUnionDefinitionFromPos:
  codeGenService: ICodeGenerationService ->
  pos: Position ->
  document: Document ->
    Async<(PatternMatchExpr * FSharpEntity * UnionMatchCasesInsertionParams) option>

val formatMatchExpr:
  insertionParams: UnionMatchCasesInsertionParams ->
  caseDefaultValue: string ->
  patMatchExpr: PatternMatchExpr ->
  entity: FSharpEntity ->
    string

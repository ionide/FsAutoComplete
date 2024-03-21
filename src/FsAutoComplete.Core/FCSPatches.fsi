/// this file contains patches to the F# Compiler Service that have not yet made it into
/// published nuget packages.  We source-copy them here to have a consistent location for our to-be-removed extensions
module FsAutoComplete.FCSPatches

open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis

type LanguageFeatureShim =
  new: langFeature: string -> LanguageFeatureShim
  member Case: obj option
  static member Type: System.Type

type LanguageVersionShim =
  new: versionText: string -> LanguageVersionShim
  member IsPreviewEnabled: bool
  member SupportsFeature: featureId: LanguageFeatureShim -> bool

module LanguageVersionShim =
  val defaultLanguageVersion: Lazy<LanguageVersionShim>

  /// <summary>Tries to parse out "--langversion:" from OtherOptions if it can't find it, returns defaultLanguageVersion</summary>
  /// <param name="fpo">The FSharpProjectOptions to use</param>
  /// <returns>A LanguageVersionShim from the parsed "--langversion:" or defaultLanguageVersion </returns>
  val fromFSharpProjectOptions: fpo: FSharpProjectOptions -> LanguageVersionShim
  val fromFSharpProjectSnapshot: fpo: FSharpProjectSnapshot -> LanguageVersionShim

module SyntaxTreeOps =
  val synExprContainsError: SynExpr -> bool

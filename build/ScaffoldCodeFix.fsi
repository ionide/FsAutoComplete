module ScaffoldCodeFix

/// Scaffold a new CodeFix by:
/// - Generating the implementation and signature files.
/// - Wire up the codefix AdaptiveFSharpLspServer.fs and FsAutoComplete.Lsp.fs.
/// - Generate a tests file with a focused test.
/// - Wire up the tests file.
/// - Update the last write time the project files.
val scaffold: codeFixName: string -> unit

/// Verifies that the code fix scaffold target can still wire up a new codefix to the existing list.
/// <exception cref="System.Exception">Throws when any expected AST nodes can no longer be found.</exception>
/// <remarks>If this code throws, you may need to revisit ScaffoldCodeFix.fs to tweak any recent changes.</remarks>
val ensureScaffoldStillWorks: unit -> unit

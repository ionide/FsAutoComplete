module ScaffoldCodeFix

/// Scaffold a new CodeFix by:
/// - Generating the implementation and signature files.
/// - Wire up the codefix AdaptiveFSharpLspServer.fs and FsAutoComplete.Lsp.fs.
/// - Generate a tests file with a focused test.
/// - Wire up the tests file.
/// - Update the last write time the project files.
val scaffold: codeFixName: string -> unit

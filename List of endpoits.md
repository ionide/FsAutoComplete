**LSP**

* `initialize` - OK
* `initialized` - OK
* `textDocument/hover` - OK
* `textDocument/didOpen` - OK
* `textDocument/didChange` - OK
* `textDocument/completion` - OK
* `completionItem/resolve `- OK
* `textDocument/rename` - OK
* `textDocument/definition` - OK
* `textDocument/typeDefinition` - OK
* `textDocument/implementation` - ?
* `textDocument/codeAction` - TODO
* `textDocument/codeLens `- OK
* `codeLens/resolve` - OK
* `textDocument/references` - OK
* `textDocument/documentHighlight` - OK
* `textDocument/documentLink` - TODO
* `documentLink/resolve` - Nope
* `textDocument/signatureHelp` - OK
* `textDocument/documentColor` - Nope
* `textDocument/colorPresentation` - Nope
* `textDocument/formatting` - ?
* `textDocument/rangeFormatting` - ?
* `textDocument/onTypeFormatting` - ?
* `textDocument/willSave` - Nope
* `textDocument/willSaveWaitUntil` - Nope
* `textDocument/didSave` - OK
* `textDocument/didClose `- Nope
* `textDocument/documentSymbol` - OK
* `workspace/didChangeWatchedFiles` - OK
* `workspace/didChangeWorkspaceFolders` - Nope
* `workspace/didChangeConfiguration` - OK
* `workspace/symbol` - OK
* `workspace/executeCommand` - TODO (`fsharp.generateDoc`)

* `window/showMessage` - OK
* `window/logMessage` - OK
* `textDocument/publishDiagnostics` - OK

**CUSTOM**

* `fsharp/signature` - OK
* `fsharp/lineLens` - OK
* `lineLens/resolve` - OK
* `fsharp/workspaceLoad` - OK
* `fsharp/compilerLocation` - OK
* `fsharp/project` - OK
* `fsharp/compile` - OK
* `fsharp/notifyWorkspace` - OK
* `fsharp/notifyWorkspacePeek` - OK


NOTES:

* Current implementation was done was trying to do minimal changes in the other layers of FSAC to ensure it doesn't break other clients. As such, some implementations in LSP layer looks bit clunky. If we agree that LSP is way to go for all clients, we can deprecate other communication protocols, and do bigger refactoring of `Core` project.
* Client is responsible for project loading/parsing/refreshing other than initial loading
* Initial project loading is done automatically if and only if there exists single solution file in the workspace, or there is no solution files in the workspace (in such case all detected projects are loaded)
* Custom endpoints are using (for messages body) `PlainNotification` type and string format serialized with exactly same serialization format as old JSON protocol
* `workspace/didChangeWatchedFiles` should be used only for removing diagnostics from the files that were deleted.
* Ionide's LineLenses needs to be implemented in client side using custom endpoints, it's too innovative for MSFT ;]
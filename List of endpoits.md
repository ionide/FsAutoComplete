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
* `textDocument/onTypeFormatting` -?
* `textDocument/willSave` - Nope
* `textDocument/willSaveWaitUntil` - Nope
* `textDocument/didSave` - OK
* `textDocument/didClose `- Nope
* `textDocument/documentSymbol` - OK
* `workspace/didChangeWatchedFiles` - TODO
* `workspace/didChangeWorkspaceFolders` - Nope
* `workspace/didChangeConfiguration` - TODO
* `workspace/symbol` - OK
* `workspace/executeCommand` - TODO (`fsharp.generateDoc`, `fsharp.clearCache`)

* `window/showMessage` - OK
* `window/logMessage` - OK
* `textDocument/publishDiagnostics` - OK

**CUSTOM**

* `fsharp/signature` - TODO
* `fsharp/lineLens` - TODO
* `fsharp/workspaceLoad` - TODO
* `fsharp/compilerLocation` - TODO
* `fsharp/project` - TODO
* `fsharp/notifyWorkspace` - OK
* `fsharp/notifyWorkspacePeek` - OK

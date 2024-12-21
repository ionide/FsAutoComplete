# Creating a New Code Fix

A code fix, also referred to as a quick fix or code action, serves as a mechanism within the editor to propose and implement code changes within the current file. 
This functionality is facilitated through the [Code Action Request](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_codeAction) provided by the Language Server Protocol (LSP). 
You might associate code fixes with the concept of "light bulbs" found in certain integrated development environments (IDEs).

To introduce a new code fix within the context of FSAutocomplete, there are several essential components to consider:

1. **Code Fix File**: This pertains to the actual implementation of the code fix.

2. **Registration in LSP Server**: Registration of the code fix is required in the associated LSP server.

3. **Unit Test Setup**: Proper unit tests need to be established to ensure the correctness and effectiveness of the new code fix.

To streamline the process of creating a new code fix, a convenient `FAKE` target has been provided. By executing the following command:

```bash
dotnet fsi build.fsx -- -p ScaffoldCodeFix YourCodeFixName
```

The above command accomplishes the following tasks:

- Generation of three files:
  - The implementation file for your code fix.
  - A signature file associated with your code fix.
  - A dedicated standalone unit test file.

Furthermore, this command updates the following files to properly register the new code fix:

- `src/FsAutoComplete/LspServers/AdaptiveState.fs`
- `test/FsAutoComplete.Tests.Lsp/CodeFixTests/Tests.fs`

The unit test file contains a [single focused test](https://github.com/haf/expecto#focusing-tests), allowing you to promptly verify the functionality. To run this initial test, you have two options:

1. Using the `dotnet test` command:
 ```bash
dotnet test -f net8.0 ./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj
 ```

2. Alternatively, using the `dotnet run` command:
 ```bash
dotnet run -f net8.0 --project ./test/FsAutoComplete.Tests.Lsp/FsAutoComplete.Tests.Lsp.fsproj
 ```

This comprehensive approach ensures that the newly introduced code fix is properly integrated, tested, and ready for seamless integration into the FSAutocomplete environment.

When preparing to submit a pull request, please consider the following guidelines:

- Eliminate any extraneous code or comments that may remain from the sample code.
- Ensure proper source code formatting by running the command `dotnet fantomas src`. 
- Avoid including focused tests, as they can cause the continuous integration build to fail.

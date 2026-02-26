
# FsAutoComplete

[![NuGet version (FsAutoComplete)](https://img.shields.io/nuget/v/FsAutoComplete.svg?style=flat-square)](https://www.nuget.org/packages/FsAutoComplete/)

The `FsAutoComplete` project (`FSAC`) provides a backend service for rich editing or intellisense features for editors.

It can be hosted using the Language Server Protocol.

Currently, it is used by the following extensions:

* [Emacs](https://github.com/fsharp/emacs-fsharp-mode)
* [Neovim](https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#fsautocomplete)
* [Vim](https://github.com/fsharp/vim-fsharp)
* [Visual Studio Code](https://github.com/ionide/ionide-vscode-fsharp)
* [Sublime Text](https://lsp.sublimetext.io/language_servers/#f)
* [Zed](https://github.com/nathanjcollins/zed-fsharp)

And it can be used with the following editors, by simply installing FsAutoComplete directly: 
`dotnet tool install --global fsautocomplete`

* [Kate](https://kate-editor.org/)
* [Flow](https://flow-control.dev/)
* [Helix](https://helix-editor.com/)

It is based on:

* [FSharp.Compiler.Service](https://github.com/fsharp/FSharp.Compiler.Service/) for F# language info.
* [Ionide.ProjInfo](https://github.com/ionide/proj-info) for project/solution management.
* [FSharpLint](https://github.com/fsprojects/FSharpLint/) for the linter feature.
* [Fantomas](https://github.com/fsprojects/fantomas) for F# code formatting.

## Building, Testing, and Releasing

See [CONTRIBUTING.md](./CONTRIBUTING.md) for build requirements, build commands, testing instructions, and the release process.


## OpenTelemetry

See the [OpenTelemetry documentation](./docs/opentelemetry.md) for instructions on enabling and visualizing distributed traces.


## Communication Protocol

See the [Communication Protocol documentation](./docs/communication-protocol.md) for details on supported LSP endpoints, custom endpoints, notifications, startup options, initialization options, and settings.

## Maintainers

The maintainers of this repository are:

* [Krzysztof Cieślak](http://github.com/Krzysztof-Cieslak)
* [Chester Husk](http://github.com/baronfel)
* [Jimmy Byrd](https://github.com/TheAngryByrd)

Previous maintainers:

* [Robin Neatherway](https://github.com/rneatherway)
* [Steffen Forkmann](http://github.com/forki)
* [Karl Nilsson](http://github.com/kjnilsson)
* [Enrico Sada](http://github.com/enricosada)

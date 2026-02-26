
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

## Documentation 

* Building, Testing, and Releasing- See [CONTRIBUTING.md](./CONTRIBUTING.md) for build requirements, build commands, testing instructions, and the release process.
* OpenTelemetry -See the [OpenTelemetry documentation](./docs/opentelemetry.md) for instructions on enabling and visualizing distributed traces.
* LSP endpoints, settings and more - See the [Communication Protocol documentation](./docs/communication-protocol.md) for details on supported LSP endpoints, custom endpoints, notifications, startup options, initialization options, and settings.

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
* [Tomas Petricek](https://github.com/tpetricek)
* [Dave Thomas](https://github.com/7sharp9)

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](https://github.com/ionide/.github/blob/master/LCODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Copyright

The library is available under [MIT license](https://github.com/ionide/FsAutoComplete/blob/master/LICENSE.md), which allows modification and redistribution for both commercial and non-commercial purposes.

### Imposter Syndrome Disclaimer

I want your help. *No really, I do*.

There might be a little voice inside that tells you you're not ready; that you need to do one more tutorial, or learn another framework, or write a few more blog posts before you can help me with this project.

I assure you, that's not the case.

And you don't just have to write code. You can help out by writing documentation, tests, or even by giving feedback about this work. (And yes, that includes giving feedback about the contribution guidelines.)

Thank you for contributing!

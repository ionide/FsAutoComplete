[![Build status](https://ci.appveyor.com/api/projects/status/qe58l7mh4e4e2a7j/branch/master?svg=true)](https://ci.appveyor.com/project/fsautocomplete/fsautocomplete/branch/master)
[![Build Status](https://travis-ci.org/fsharp/FsAutoComplete.svg?branch=master)](https://travis-ci.org/fsharp/FsAutoComplete)

# FsAutoComplete

The `FsAutoComplete` project (`FSAC`) provides a backend service for rich editing or 'intellisense' features for editors.

It can be hosted as command-line interface (`stdio` mode) or as http server (`http` mode), both using the same json protocol.

Currently it is used by:

* [Emacs](https://github.com/fsharp/emacs-fsharp-mode)
* [Vim](https://github.com/fsharp/vim-fsharp)
* [Visual Studio Code](https://github.com/ionide/ionide-vscode-fsharp)

It's based on:

- [FSharp.Compiler.Service](https://github.com/fsharp/FSharp.Compiler.Service/) for F# language info.
- [Dotnet.ProjInfo](https://github.com/enricosada/dotnet-proj-info/) for project/sln management.
- [FSharpLint](https://github.com/fsprojects/FSharpLint/) for the linter feature.

## Required software

FsAutoComplete can run on .NET/mono or .NET Core.

### FSAC .NET

* on windows: [Microsoft Build Tools 2015](https://www.microsoft.com/en-us/download/details.aspx?id=48159)
* on unix/mac: Required: Mono >= 5.12, Recommended: Mono >= 5.18

### FSAC .NET Core

* .NET Core Sdk
* on unix/mac: Required: Mono >= 5.12, Recommended: Mono >= 5.18

## Building and testing

Requirements:

- .NET Core Sdk, see [global.json](global.json) for the exact version.
- Mono 5.18 on unix/osx
- Microsoft Build Tools 2013

There is a [FAKE script](build.fsx) who can be invoked with `build.cmd`/`build.sh`.

- To build fsautocomplete binaries in `~/bin` directory, do run `build LocalRelease`
- To build, run all tests and create packages, do run `build All`

The [integration tests](test/FsAutoComplete.IntegrationTests) use a simple strategy of running a scripted session with `fsautocomplete` and then comparing the output with that saved in the repository. This requires careful checking when the test is first constructed. On later runs, absolute paths are removed using regular expressions to ensure that the tests are machine-independent.

see [test/FsAutoComplete.IntegrationTests/README.md](test/FsAutoComplete.IntegrationTests/README.md) for more info.

## Troubleshooting

### FileWatcher exceptions

You may see a stack trace finishing with `System.IO.IOException: kqueue() error at init, error code = ’0’`. This is due to a limitation in the number of filehandles that the Mono file watchers can keep open. Restarting FsAutoComplete or the hosting editor should help. If not, try setting `export MONO_MANAGED_WATCHER=disabled` in your `~/.bash_profile`. Note that on OSX, this setting will only take effect if you launch emacs from the terminal.

## Communication protocol

It is expected that the editor will launch this program in the background and communicate over a pipe. It is possible to use interactively, although due to the lack of any readline support it isn't pleasant, and text pasted into the buffer may not be echoed. As a result, use this only for very simple debugging. For more complex scenarios it is better to write another integration test by copying an [existing one](test/FsAutoComplete.IntegrationTests/Test1Json).

The available commands can be listed by running `fsautocomplete --commands`. Commands are all on a single line, with the exception of the `parse` command, which should be followed by the current text of the file to parse (which may differ from the contents on disk), and terminated with a line containing only `<<EOF>>`.

Data is returned as JSON. An example of a simple session is:

```
project "Test1.fsproj"
```

```json
{"Kind":"project","Data":{"Files":["<absolute path removed>/Program.fs"],"Output":"<absolute path removed>/bin/Debug/Test1.exe"}}
```

```
parse "Program.fs"
module X =
    let func x = x + 1

    let val2 = X.func 2
    <<EOF>>
```

```json
{"Kind":"INFO","Data":"Background parsing started"}
completion "Program.fs" 4 13
{"Kind":"completion","Data":["func"]}
```

Each response is exactly one line, which simplifies the application of a JSON parser. For further insight into the communication protocol, have a look over the integration tests, which have examples of all the features. Each folder contains one or more `*Runner.fsx` files which specify a sequence of commands to send, and `*.json` files, which contain the output.

## Maintainers

The maintainers of this repository are:

- [Steffen Forkmann](http://github.com/forki)
- [Karl Nilsson](http://github.com/kjnilsson)
- [Enrico Sada](http://github.com/enricosada)
- [Krzysztof Cieślak](http://github.com/Krzysztof-Cieslak)

The primary maintainer for this repository is [Enrico Sada](http://github.com/enricosada)

Previous maintainers:

- [Robin Neatherway](https://github.com/rneatherway)

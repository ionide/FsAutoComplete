module FsAutoComplete.Tests.Lsp

open Expecto
open OmniSharp.Extensions.LanguageServer.Client

let tests =
  test "A simple test" {
    let subject = "Hello World"
    Expect.equal subject "Hello World" "The strings should equal"
  }
// --------------------------------------------------------------------------------------
// (c) Robin Neatherway
// --------------------------------------------------------------------------------------
namespace FsAutoComplete

open System
open System.Diagnostics
open System.Reflection

module Version =
  let string = "FsAutoComplete " + AssemblyVersionInformation.Version

module Options =

  let commandText = @"
    Supported commands
    ==================
    quit
      - quit the program
    declarations ""filename""
      - get information about top-level declarations in a file with location
    parse ""<filename>"" [sync]
      - trigger full background parse request; should be
        followed by content of a file (ended with <<EOF>>)
        Optional 'sync' is used to force the parse to occur
        synchronously for testing purposes. Not intended for
        use in production.
    completion ""<filename>"" <line> <col> [timeout] [filter=(StartsWith|Contains)]
      - trigger completion request for the specified location
        optionally filter in the specified manner
    helptext <candidate>
      - fetch type signature for specified completion candidate
        (from last completion request). Only use in JSON mode.
    symboluse ""<filename>"" <line> <col> [timeout]
      - find all uses of the symbol for the specified location
    tooltip ""<filename>"" <line> <col> [timeout]
      - get tool tip for the specified location
    finddecl ""<filename>"" <line> <col> [timeout]
      - find the point of declaration of the symbol at specified location
    methods ""<filename>"" <line> <col> [timeout]
      - find the method signatures at specified location
    project ""<filename>""
      - associates the current session with the specified project
    compilerlocation
      - prints the best guess for the location of fsc and fsi
        (or fsharpc and fsharpi on unix)
    "
  let verbose = ref false
  let timeout = ref 10

  let verboseFilter : Ref<Option<Set<string>>> = ref None

  let p = new NDesk.Options.OptionSet()
  Seq.iter (fun (s:string,d:string,a:string -> unit) -> ignore (p.Add(s,d,a)))
    [
      "version", "display versioning information",
        fun _ -> printfn "%s" Version.string;
                 exit 0

      "v|verbose", "enable verbose mode",
        fun _ -> Debug.verbose := true

      "l|logfile=", "send verbose output to specified log file",
        fun s -> try
                   Debug.output := (IO.File.CreateText(s) :> IO.TextWriter)
                 with
                   | e -> printfn "Bad log file: %s" e.Message
                          exit 1

      "vfilter=", "apply a comma-separated {FILTER} to verbose output",
        fun v -> Debug.categories := v.Split(',') |> set |> Some

      "commands", "list the commands that this program understands",
        fun _ -> printfn "%s" commandText
                 exit 0

      "h|?|help", "display this help!",
        fun _ -> printfn "%s" Version.string;
                 p.WriteOptionDescriptions(stdout);
                 exit 0
    ]

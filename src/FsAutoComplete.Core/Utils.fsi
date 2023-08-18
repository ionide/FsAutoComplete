[<AutoOpen>]
module FsAutoComplete.Utils

open System.Diagnostics
open System.Threading.Tasks
open System.IO
open System.Collections.Concurrent
open System
open FSharp.Compiler.CodeAnalysis
open FSharp.UMX
open FSharp.Compiler.Symbols
open System.Runtime.CompilerServices

/// Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources.
val dispose: d: #IDisposable -> unit
/// <summary>Performs application-defined tasks associated with freeing, releasing, or resetting unmanaged resources asynchronously.</summary>
/// <returns>A task that represents the asynchronous dispose operation.</returns>
val disposeAsync: d: #IAsyncDisposable -> ValueTask

module Map =
  /// Combine two maps of identical types by starting with the first map and overlaying the second one.
  /// Because map updates shadow, any keys in the second map will have priority.
  val merge: first: Map<'a, 'b> -> second: Map<'a, 'b> -> Map<'a, 'b> when 'a: comparison
  /// Combine two maps by taking the first value of each key found.
  val combineTakeFirst: first: Map<'a, 'b> -> second: Map<'a, 'b> -> Map<'a, 'b> when 'a: comparison
  val values: m: Map<'a, 'b> -> seq<'b> when 'a: comparison

module Seq =
  val intersperse: separator: 'a -> sequence: #seq<'a> -> seq<'a>

module ProcessHelper =
  val WaitForExitAsync: p: Process -> Async<unit>

type ResultOrString<'a> = Result<'a, string>
type Serializer = obj -> string
type ProjectFilePath = string
type SourceFilePath = string
type FilePath = string
type LineStr = string

/// OS-local, normalized path
[<Measure>]
type LocalPath

/// An HTTP url
[<Measure>]
type Url

/// OS-Sensitive path segment from some repository root
[<Measure>]
type RepoPathSegment

[<Measure>]
type NormalizedRepoPathSegment

type Document =
  { FullName: string<LocalPath>
    LineCount: int
    GetText: unit -> string
    GetLineText0: int -> string
    GetLineText1: int -> string }

/// <summary>
/// Checks if the file ends with `.fsx` `.fsscript` or `.sketchfs`
/// </summary>
val inline isAScript: fileName: ReadOnlySpan<char> -> bool
/// <summary>
/// Checks if the file ends with `.fsi`
/// </summary>
val inline isSignatureFile: fileName: ReadOnlySpan<char> -> bool
/// <summary>
/// Checks if the file ends with `.fs`
/// </summary>
val isFsharpFile: fileName: ReadOnlySpan<char> -> bool
/// <summary>
/// This is a combination of `isAScript`, `isSignatureFile`, and `isFsharpFile`
/// </summary>
/// <param name="fileName"></param>
/// <returns></returns>
val inline isFileWithFSharp: fileName: string -> bool
val inline normalizePath: file: string -> string<LocalPath>
val inline combinePaths: path1: string -> path2: string -> string
val inline (</>): path1: string -> path2: string -> string
val projectOptionsToParseOptions: checkOptions: FSharpProjectOptions -> FSharpParsingOptions

[<RequireQualifiedAccess>]
module Option =
  val inline attempt: f: (unit -> 'T) -> 'T option
  /// ensure the condition is true before continuing
  val inline guard: b: bool -> unit option

[<RequireQualifiedAccess>]
module Result =
  val inline bimap: okF: ('a -> 'b) -> errF: ('c -> 'b) -> r: Result<'a, 'c> -> 'b
  val inline ofOption: recover: (unit -> 'a) -> o: 'b option -> Result<'b, 'a>
  val inline ofVOption: recover: (unit -> 'a) -> o: 'b voption -> Result<'b, 'a>
  /// ensure the condition is true before continuing
  val inline guard: condition: (unit -> bool) -> errorValue: 'a -> Result<unit, 'a>

[<RequireQualifiedAccess>]
module Async =
  /// Transforms an Async value using the specified function.
  [<CompiledName("Map")>]
  val map: mapping: ('a -> 'b) -> value: Async<'a> -> Async<'b>

  [<CompiledName("Bind")>]
  val bind: binding: ('a -> Async<'b>) -> value: Async<'a> -> Async<'b>

  val StartCatchCancellation: work: Async<'a> * cancellationToken: Threading.CancellationToken -> Async<'a>
  /// <summary>Creates an asynchronous computation that executes all the given asynchronous computations, using 75% of the Environment.ProcessorCount</summary>
  /// <param name="computations">A sequence of distinct computations to be parallelized.</param>
  val parallel75: computations: seq<Async<'a>> -> Async<'a array>

  [<RequireQualifiedAccess>]
  module Array =
    /// Async implementation of Array.map.
    val map: mapping: ('T -> Async<'U>) -> array: 'T[] -> Async<'U array>

[<RequireQualifiedAccess>]
module AsyncResult =
  val inline bimap: okF: ('a -> 'b) -> errF: ('c -> 'b) -> r: Async<Result<'a, 'c>> -> Async<'b>
  val inline ofOption: recover: (unit -> 'a) -> o: Async<'b option> -> Async<Result<'b, 'a>>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =
  /// Optimized arrays equality. ~100x faster than `array1 = array2` on strings.
  /// ~2x faster for floats
  /// ~0.8x slower for ints
  val inline areEqual: xs: 'T[] -> ys: 'T[] -> bool when 'T: equality
  /// Fold over the array passing the index and element at that index to a folding function
  val foldi: folder: ('State -> int -> 'T -> 'State) -> state: 'State -> array: 'T[] -> 'State
  /// Returns all heads of a given array.
  /// For [|1;2;3|] it returns [|[|1; 2; 3|]; [|1; 2|]; [|1|]|]
  val heads: array: 'T[] -> 'T array array
  /// check if subArray is found in the wholeArray starting
  /// at the provided index
  val inline isSubArray: subArray: 'T[] -> wholeArray: 'T[] -> index: int -> bool when 'T: equality
  /// Returns true if one array has another as its subset from index 0.
  val startsWith: prefix: 'a array -> whole: 'a array -> bool when 'a: equality
  /// Returns true if one array has trailing elements equal to another's.
  val endsWith: suffix: 'a array -> whole: 'a array -> bool when 'a: equality
  /// Returns a new array with an element replaced with a given value.
  val replace: index: int -> value: 'a -> array: 'a array -> 'a array
  /// pass an array byref to reverse it in place
  val revInPlace: array: 'T[] -> unit when 'T: equality
  val splitAt: n: int -> xs: 'a[] -> 'a array * 'a array
  val partitionResults: xs: Result<'a, 'b> array -> 'a array * 'b array

module List =
  ///Returns the greatest of all elements in the list that is less than the threshold
  val maxUnderThreshold: nmax: int -> (int list -> int)
  /// Groups a tupled list by the first item to produce a list of values
  val groupByFst: tupledItems: ('Key * 'Value) list -> ('Key * 'Value list) list when 'Key: equality

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =
  /// Concatenates all the elements of a string array, using the specified separator between each element.
  val inline join: separator: string -> items: string seq -> string
  val inline toCharArray: str: string -> char array
  val lowerCaseFirstChar: str: string -> string
  val extractTrailingIndex: str: string -> string * int option
  val (|StartsWith|_|): pattern: string -> value: string -> unit option
  val split: splitter: char -> s: string -> string list
  val getLines: str: string -> string array

  type SplitResult =
    | NoMatch
    | Split of left: string * right: string

  val splitAtChar: splitter: char -> s: string -> SplitResult

[<Class>]
[<Extension>]
type ReadOnlySpanExtensions =
  /// Note: empty string -> 1 line
  [<Extension>]
  static member CountLines: text: ReadOnlySpan<char> -> int

  [<Extension>]
  static member LastLine: text: ReadOnlySpan<char> -> ReadOnlySpan<char>

type ConcurrentDictionary<'key, 'value> with

  member TryFind: key: 'key -> 'value option

type Path with

  static member GetFullPathSafe: path: string -> string
  static member GetFileNameSafe: path: string -> string
  static member LocalPathToUri: filePath: string<LocalPath> -> string
  /// Algorithm from https://stackoverflow.com/a/35734486/433393 for converting file paths to uris,
  /// modified slightly to not rely on the System.Path members because they vary per-platform
  static member FilePathToUri: filePath: string -> string
  /// handles unifying the local-path logic for windows and non-windows paths,
  /// without doing a check based on what the current system's OS is.
  static member FileUriToLocalPath: uriString: string -> string

val inline debug: msg: Printf.StringFormat<'a, unit> -> 'a
val inline fail: msg: Printf.StringFormat<'a, unit> -> 'a
val chooseByPrefix: prefix: string -> s: string -> string option
val chooseByPrefix2: prefixes: string list -> s: string -> string option
val splitByPrefix: prefix: string -> s: string -> (string * string) option
val splitByPrefix2: prefixes: string list -> s: string -> (string * string) option

[<AutoOpen>]
module Patterns =
  val (|StartsWith|_|): pat: string -> str: string -> string option
  val (|Contains|_|): pat: string -> str: string -> string option

module Version =
  open System.Reflection
  type VersionInfo = { Version: string; GitSha: string }
  val info: unit -> VersionInfo

type Debounce<'a> =
  new: timeout: int * fn: ('a -> Async<unit>) -> Debounce<'a>
  /// Calls the function, after debouncing has been applied.
  member Bounce: arg: 'a -> unit
  /// Timeout in ms
  member Timeout: int with get, set

module Indentation =
  val inline get: line: string -> int

type FSharpSymbol with

  member inline XDoc: FSharpXmlDoc
  member inline XSig: string
  member inline DefinitionRange: FSharp.Compiler.Text.range

module Tracing =
  open System.Diagnostics
  open FsAutoComplete.Telemetry
  open StreamJsonRpc
  open System.Collections.Generic

  module SemanticConventions =
    /// <remarks>
    /// <see href="https://github.com/dotnet/fsharp/blob/c68285b00e2f53607d836aa0a11ab21abd556842/src/Compiler/Utilities/Activity.fs#L13"> From F# compiler</see>
    /// </remarks>
    module FCS =
      [<Literal>]
      val fileName: string = "fileName"

      [<Literal>]
      val project: string = "project"

      [<Literal>]
      val qualifiedNameOfFile: string = "qualifiedNameOfFile"

      [<Literal>]
      val userOpName: string = "userOpName"

    [<Literal>]
    val fsac_sourceCodePath: string = "fsac.sourceCodePath"

    [<Literal>]
    val projectFilePath: string = "fsac.projectFilePath"

  [<Literal>]
  val serviceName: string = "FsAutoComplete"

  /// <remarks>
  /// <see href="https://github.com/dotnet/fsharp/blob/c68285b00e2f53607d836aa0a11ab21abd556842/src/Compiler/Utilities/Activity.fs#L43"> From F# compiler</see>
  /// </remarks>
  [<Literal>]
  val fscServiceName: string = "fsc"

  val fsacActivitySource: ActivitySource
  val recordException: e: exn -> trace: Activity -> unit

  /// <summary>
  /// StreamJsonRpcTracingStrategy participates in and propagates trace context in  vs-streamjsonrpc
  /// </summary>
  ///
  /// <remarks>
  /// <see href="https://github.com/microsoft/vs-streamjsonrpc/blob/main/doc/tracecontext.md"> vs-streamjsonrpc tracecontext documentation</see>
  /// </remarks>
  type StreamJsonRpcTracingStrategy =
    new: activitySource: ActivitySource -> StreamJsonRpcTracingStrategy
    interface IActivityTracingStrategy

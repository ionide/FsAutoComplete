/// Adapters for the parts of `Fantomas.Client` that report outside a `FantomasResponse`: the log
/// delegate it hands its own progress to, and the configuration warnings a Fantomas 8 daemon raises
/// for every format request.
module FsAutoComplete.Lsp.FantomasReporting

open System
open System.IO
open FsAutoComplete.Logging
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes

/// Write what Fantomas.Client reports about itself - which Fantomas a folder resolved to and where
/// it was found, and whether a daemon could be started - to the logger. None of that fits in a
/// FantomasResponse, and the package hands it over as a delegate rather than through a logging
/// abstraction, so that it keeps its three dependencies.
let logMessage (logger: ILog) (level: FantomasLogLevel) (message: string) =
  let config =
    Log.setMessage "{fantomasMessage}" >> Log.addContext "fantomasMessage" message

  match level with
  | FantomasLogLevel.Debug -> logger.debug config
  | FantomasLogLevel.Warning -> logger.warn config
  | FantomasLogLevel.Error -> logger.error config
  | FantomasLogLevel.Info
  | _ -> logger.info config

/// One sentence about a setting in the resolved configuration that Fantomas could not act on.
/// Formatting still succeeded; the setting simply did not apply.
let describeConfigurationProblem (problem: ConfigurationProblem) =
  let source =
    match enum<ConfigurationProblemSource> problem.Source with
    | ConfigurationProblemSource.Request -> "the format request"
    | ConfigurationProblemSource.EditorConfig
    | _ -> ".editorconfig"

  match enum<ConfigurationProblemCode> problem.Code with
  | ConfigurationProblemCode.UnrecognizedValue ->
    let value = if isNull problem.Value then "" else problem.Value

    sprintf
      "\"%s\" in %s has the value \"%s\", which Fantomas could not read, so its default was used"
      problem.Setting
      source
      value
  | ConfigurationProblemCode.UnknownSetting
  | _ -> sprintf "\"%s\" in %s is not a setting Fantomas has" problem.Setting source

/// The Fantomas that raised a warning, named the way it goes in front of a user: `Fantomas
/// 8.0.0-alpha-022`. The version arrives ready to show - no `v`, no `+<commit>` - because
/// `Fantomas.Client` stamps it on from the shape it caches daemons under. It is absent only for a
/// caller driving a daemon itself rather than going through `LSPFantomasService`, which is not us,
/// but a missing version is no reason to withhold the rest of the message.
let describeFantomas (warning: ConfigurationWarning) =
  if isNull warning.Version then
    "Fantomas"
  else
    sprintf "Fantomas %s" warning.Version

/// `.editorconfig`, or `<parent>/.editorconfig` when several contributed and the bare name would
/// not say which is which.
let private editorConfigLabel (qualify: bool) (path: string) =
  let name = Path.GetFileName path

  if not qualify then
    name
  else
    match Path.GetDirectoryName path with
    | null -> name
    | directory -> sprintf "%s/%s" (Path.GetFileName directory) name

/// The files a configuration warning is worth opening, each with the title of the action that
/// opens it: every `.editorconfig` that contributed, and the file that was being formatted.
///
/// All of the `.editorconfig` files are offered rather than the one at fault, because which of
/// them a given problem came from is not knowable: editorconfig merges the whole chain into one
/// set of properties before Fantomas sees it.
let documentsToOpen (warning: ConfigurationWarning) =
  let qualify = warning.EditorConfigFiles.Length > 1

  [ for editorConfigFile in warning.EditorConfigFiles do
      sprintf "Open %s" (editorConfigLabel qualify editorConfigFile), editorConfigFile

    sprintf "Open %s" (Path.GetFileName warning.FilePath), warning.FilePath ]

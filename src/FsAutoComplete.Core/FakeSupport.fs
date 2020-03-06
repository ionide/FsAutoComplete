namespace FsAutoComplete

open Fake.Runtime
open FsAutoComplete.Logging

module FakeSupport =
  let getFakeRuntime () =
      Tooling.getFakeRuntime()

  type Declaration = Tooling.Declaration
  type FakeContext = Tooling.FakeContext
  type Dependency = Tooling.Dependency
  type Target = Tooling.Target

  let private logger = LogProvider.getLoggerByName "FakeSupport"

  let private setupLogging () =
    Tooling.setupLogging (fun isError ->
      let logger = if isError then logger.error else logger.info
      Log.setMessage >> logger
    )

  let detectFakeScript (file) =
    setupLogging()
    Tooling.detectFakeScript file

  let getProjectOptions (detectionInfo:Tooling.DetectionInfo) =
    setupLogging()
    Tooling.getProjectOptions detectionInfo

  type GetTargetsWarningOrErrorType = Tooling.GetTargetsWarningOrErrorType

  type GetTargetsResult = Tooling.GetTargetsResult

  let getTargets (file:string) (ctx:FakeContext) : Async<GetTargetsResult> =
    setupLogging()
    Tooling.getTargets file ctx

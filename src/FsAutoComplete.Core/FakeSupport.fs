namespace FsAutoComplete

open Fake.Runtime
open System
open System.IO
open System.IO.Compression
open System.Threading.Tasks
open System.Net
open Newtonsoft.Json.Linq

module FakeSupport =
  let getFakeRuntime () =
      Tooling.getFakeRuntime()  

  type Declaration = Tooling.Declaration
  type FakeContext = Tooling.FakeContext
  type Dependency = Tooling.Dependency
  type Target = Tooling.Target

  let private setupLogging () =
    Tooling.setupLogging (fun _isError -> Debug.print "%s")

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

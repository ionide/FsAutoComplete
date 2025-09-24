namespace FsAutoComplete.BuildServerProtocol

open Newtonsoft.Json.Linq

/// Core JSON RPC 2.0 protocol types for communication
module JsonRpc =

  /// JSON RPC request message
  type JsonRpcRequest =
    { Id: JToken
      Method: string
      Params: JToken option }

  /// JSON RPC response message
  type JsonRpcResponse =
    { Id: JToken option
      Result: JToken option
      Error: JsonRpcError option }

  /// JSON RPC error object
  and JsonRpcError =
    { Code: int
      Message: string
      Data: JToken option }

  /// JSON RPC notification message
  type JsonRpcNotification =
    { Method: string
      Params: JToken option }

  /// Standard JSON RPC error codes
  module ErrorCodes =
    let ParseError = -32700
    let InvalidRequest = -32600
    let MethodNotFound = -32601
    let InvalidParams = -32602
    let InternalError = -32603

    // Server error range
    let ServerErrorStart = -32099
    let ServerErrorEnd = -32000

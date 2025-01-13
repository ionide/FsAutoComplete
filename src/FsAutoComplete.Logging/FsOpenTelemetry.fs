// https://github.com/TheAngryByrd/FsOpenTelemetry/blob/fb24e8d4dc8ad14187b9b4631854fc8f17fb244d/src/FsOpenTelemetry/FsOpenTelemetry.fs

namespace FsAutoComplete.Telemetry

open System
open System.Diagnostics
open System.Runtime.CompilerServices
open System.Collections.Generic


// Thanks https://github.com/fsprojects/FSharp.UMX
[<MeasureAnnotatedAbbreviation>]
type string<[<Measure>] 'm> = string


module private Funcs =
  /// <summary>
  /// Determines whether the given value is not null.
  /// </summary>
  /// <param name="value">The value to check</param>
  /// <returns>True when value is not null, false otherwise.</returns>
  let inline isNotNull value = value |> isNull |> not

module private Unsafe =
  let inline cast<'a, 'b> (a: 'a) : 'b = (# "" a : 'b #)

type UMX =
  static member inline tag<[<Measure>] 'm>(x: string) : string<'m> = Unsafe.cast x
  static member inline untag<[<Measure>] 'm>(x: string<'m>) : string = Unsafe.cast x

  static member inline cast<[<Measure>] 'm1, [<Measure>] 'm2>(x: string<'m1>) : string<'m2> = Unsafe.cast x

/// In OpenTelemetry spans can be created freely and it’s up to the implementor to annotate them with attributes specific to the represented operation. Spans represent specific operations in and between systems. Some of these operations represent calls that use well-known protocols like HTTP or database calls. Depending on the protocol and the type of operation, additional information is needed to represent and analyze a span correctly in monitoring systems. It is also important to unify how this attribution is made in different languages. This way, the operator will not need to learn specifics of a language and telemetry collected from polyglot (multi-language) micro-service environments can still be easily correlated and cross-analyzed.
module SemanticConventions =
  /// The attributes described in this section are not specific to a particular operation but rather generic. They may be used in any Span they apply to. Particular operations may refer to or require some of these attributes.
  module General =
    /// These attributes may be used for any network related operation. The net.peer.* attributes describe properties of the remote end of the network connection (usually the transport-layer peer, e.g. the node to which a TCP connection was established), while the net.host.* properties describe the local end. In an ideal situation, not accounting for proxies, multiple IP addresses or host names, the net.peer.* properties of a client are equal to the net.host.* properties of the server and vice versa.
    ///
    /// https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/span-general.md#general-network-connection-attributes
    module Network =
      /// Transport protocol used.
      ///
      /// ValueType: string
      ///
      /// Examples: ip_tcp
      ///
      /// Should use a net_transport_values
      ///
      /// Required: No
      [<Literal>]
      let net_transport = "net.transport"

      /// net.transport MUST be one of the following
      [<Measure>]
      type net_transport_values

      /// tcp_ip
      let net_transport_values_ip_tcp: string<net_transport_values> = UMX.tag "icp_tcp"
      /// ip_udp
      let net_transport_values_ip_udp: string<net_transport_values> = UMX.tag "ip_udp"
      // /// Another IP-based protocol
      let net_transport_values_ip: string<net_transport_values> = UMX.tag "ip"
      /// Unix Domain socket.
      let net_transport_values_unix: string<net_transport_values> = UMX.tag "unix"
      // Named or anonymous pipe.
      let net_transport_values_pipe: string<net_transport_values> = UMX.tag "pipe"
      ///Signals that there is only in-process communication not using a "real" network protocol in cases where network attributes would normally be expected. Usually all other network attributes can be left out in that case.
      let net_transport_values_inproc: string<net_transport_values> = UMX.tag "inproc"
      /// Something else (non IP-based).
      let net_transport_values_Other: string<net_transport_values> = UMX.tag "Other"

      /// Remote address of the peer (dotted decimal for IPv4 or RFC5952 for IPv6)
      ///
      /// ValueType: string
      ///
      /// Examples: 127.0.0.1
      ///
      /// Required: No
      [<Literal>]
      let net_peer_ip = "net.peer.ip"

      /// Remote port number.
      ///
      /// ValueType: int
      ///
      /// Examples: 80; 8080; 443
      ///
      /// Required: No
      [<Literal>]
      let net_peer_port = "net.peer.port"

      /// Remote hostname or similar.
      ///
      /// ValueType: string
      ///
      /// Examples: example.com
      ///
      /// Required: No
      // TODO: "See Note below"
      [<Literal>]
      let net_peer_name = "net.peer.name"

      /// Like net.peer.ip but for the host IP. Useful in case of a multi-IP host.
      ///
      /// ValueType: string
      ///
      /// Examples: example.com
      ///
      /// Required: No
      [<Literal>]
      let net_host_ip = "net.host.ip"

      /// Like net.peer.port but for the host port.
      ///
      /// ValueType: int
      ///
      /// Examples: 80; 8080; 443
      ///
      /// Required: No
      [<Literal>]
      let net_host_port = "net.host.port"

      /// Local hostname or similar
      ///
      /// ValueType: string
      ///
      /// Examples: localhost
      ///
      /// Required: No
      // TODO: "See Note below"
      [<Literal>]
      let net_host_name = "net.host.name"

      /// The internet connection type currently being used by the host.
      ///
      /// ValueType: string
      ///
      /// Examples: wifi
      ///
      /// Required: No
      [<Literal>]
      let net_host_connection_type = "net.host.connection.type"

      /// net.host.connection.type MUST be one of the following or, if none of the listed values apply, a custom value:
      [<Measure>]
      type net_host_connection_type_values

      let net_host_connection_type_values_wifi: string<net_host_connection_type_values> =
        UMX.tag "wifi"

      let net_host_connection_type_values_wired: string<net_host_connection_type_values> =
        UMX.tag "wired"

      let net_host_connection_type_values_cell: string<net_host_connection_type_values> =
        UMX.tag "cell"

      let net_host_connection_type_values_unavailable: string<net_host_connection_type_values> =
        UMX.tag "unavailable"

      let net_host_connection_type_values_unknown: string<net_host_connection_type_values> =
        UMX.tag "unknown"


      /// This describes more details regarding the connection.type. It may be the type of cell technology connection, but it could be used for describing details about a wifi connection.
      ///
      /// ValueType: string
      ///
      /// Examples: lte
      ///
      /// Required: No
      [<Literal>]
      let net_host_connection_subtype = "net.host.connection.subtype"

      /// net.host.connection.subtype MUST be one of the following or, if none of the listed values apply, a custom value:
      [<Measure>]
      type net_host_connection_subtype_values


      let net_host_connection_subtype_values_gprs: string<net_host_connection_type_values> =
        UMX.tag "gprs"

      let net_host_connection_subtype_values_edge: string<net_host_connection_type_values> =
        UMX.tag "edge"

      let net_host_connection_subtype_values_umts: string<net_host_connection_type_values> =
        UMX.tag "umts"

      let net_host_connection_subtype_values_cdma: string<net_host_connection_type_values> =
        UMX.tag "cdma"

      let net_host_connection_subtype_values_evdo_0: string<net_host_connection_type_values> =
        UMX.tag "evdo_0"

      let net_host_connection_subtype_values_evdo_a: string<net_host_connection_type_values> =
        UMX.tag "evdo_a"

      let net_host_connection_subtype_values_cdma2000_1xrtt: string<net_host_connection_type_values> =
        UMX.tag "cdma2000_1xrtt"

      let net_host_connection_subtype_values_hsdpa: string<net_host_connection_type_values> =
        UMX.tag "hsdpa"

      let net_host_connection_subtype_values_hsupa: string<net_host_connection_type_values> =
        UMX.tag "hsupa"

      let net_host_connection_subtype_values_iden: string<net_host_connection_type_values> =
        UMX.tag "iden"

      let net_host_connection_subtype_values_ehrpd: string<net_host_connection_type_values> =
        UMX.tag "ehrpd"

      let net_host_connection_subtype_values_hspap: string<net_host_connection_type_values> =
        UMX.tag "hspap"

      let net_host_connection_subtype_values_gsm: string<net_host_connection_type_values> =
        UMX.tag "gsm"

      let net_host_connection_subtype_values_td_scdma: string<net_host_connection_type_values> =
        UMX.tag "td_scdma"

      let net_host_connection_subtype_values_iwlan: string<net_host_connection_type_values> =
        UMX.tag "iwlan"

      let net_host_connection_subtype_values_nr: string<net_host_connection_type_values> =
        UMX.tag "nr"

      let net_host_connection_subtype_values_nrnsa: string<net_host_connection_type_values> =
        UMX.tag "nrnsa"

      let net_host_connection_subtype_values_lte_ca: string<net_host_connection_type_values> =
        UMX.tag "lte_ca"


      /// The name of the mobile carrier.
      ///
      /// ValueType: string
      ///
      /// Examples: sprint
      ///
      /// Required: No
      [<Literal>]
      let net_host_carrier_name = "net.host.carrier.name"

      /// The mobile carrier country code.
      ///
      /// ValueType: string
      ///
      /// Examples: 310
      ///
      /// Required: No
      [<Literal>]
      let net_host_carrier_mcc = "net.host.carrier.mcc"

      /// The mobile carrier network code.
      ///
      /// ValueType: string
      ///
      /// Examples: 001
      ///
      /// Required: No
      [<Literal>]
      let net_host_carrier_mnc = "net.host.carrier.mnc"

      /// The ISO 3166-1 alpha-2 2-character country code associated with the mobile carrier network.
      ///
      /// ValueType: string
      ///
      /// Examples: DE
      ///
      /// Required: No
      [<Literal>]
      let net_host_carrier_icc = "net.host.carrier.icc"

    /// This attribute may be used for any operation that accesses some remote service. Users can define what the name of a service is based on their particular semantics in their distributed system. Instrumentations SHOULD provide a way for users to configure this name.
    ///
    /// https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/span-general.md#general-remote-service-attributes
    module Remote =
      /// The service.name of the remote service. SHOULD be equal to the actual service.name resource attribute of the remote service if any.
      ///
      /// ValueType: string
      ///
      /// Examples: AuthTokenCache
      ///
      /// Examples of peer.service that users may specify:
      ///
      /// A Redis cache of auth tokens as peer.service="AuthTokenCache".
      ///
      /// A gRPC service rpc.service="io.opentelemetry.AuthService" may be hosted in both a gateway, peer.service="ExternalApiService" and a backend, peer.service="AuthService".
      ///
      /// Required: No
      [<Literal>]
      let peer_service = "peer.service"

    /// These attributes may be used for any operation with an authenticated and/or authorized enduser.
    ///
    /// https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/span-general.md#general-identity-attributes
    module Identity =

      /// Username or client_id extracted from the access token or Authorization header in the inbound request from outside the system.
      ///
      /// ValueType: string
      ///
      /// Examples: username
      ///
      /// Required: No
      [<Literal>]
      let enduser_id = "enduser.id"

      /// Actual/assumed role the client is making the request under extracted from token or application security context.
      ///
      /// ValueType: string
      ///
      /// Examples: admin
      ///
      /// Required: No
      [<Literal>]
      let enduser_role = "enduser.role"

      /// Scopes or granted authorities the client currently possesses extracted from token or application security context. The value would come from the scope associated with an OAuth 2.0 Access Token or an attribute value in a SAML 2.0 Assertion.
      ///
      /// ValueType: string
      ///
      /// Examples: read:message, write:files
      ///
      /// Required: No
      [<Literal>]
      let enduser_scope = "enduser.scope"

    /// These attributes may be used for any operation to store information about a thread that started a span.
    ///
    /// https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/span-general.md#general-thread-attributes
    module Thread =

      /// Current "managed" thread ID (as opposed to OS thread ID).
      ///
      /// ValueType: int
      ///
      /// Examples: 42
      ///
      /// Can use Thread.CurrentThread.ManagedThreadId
      ///
      /// Required: No
      [<Literal>]
      let thread_id = "thread.id"

      /// Current thread name.
      ///
      /// ValueType: string
      ///
      /// Examples: main
      ///
      /// Can use Thread.CurrentThread.Name
      ///
      /// Required: No
      [<Literal>]
      let thread_name = "thread.name"

    /// Often a span is closely tied to a certain unit of code that is logically responsible for handling the operation that the span describes (usually the method that starts the span). For an HTTP server span, this would be the function that handles the incoming request, for example. The attributes listed below allow to report this unit of code and therefore to provide more context about the span.
    ///
    /// https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/span-general.md#source-code-attributes
    module SourceCode =
      /// The method or function name, or equivalent (usually rightmost part of the code unit's name).
      ///
      /// ValueType: string
      ///
      /// Examples: serveRequest
      ///
      /// Required: No
      [<Literal>]
      let code_function = "code.function"

      /// The "namespace" within which code.function is defined. Usually the qualified class or module name, such that code.namespace + some separator + code.function form a unique identifier for the code unit.
      ///
      /// ValueType: string
      ///
      /// Examples: com.example.MyHttpService
      ///
      /// Required: No
      [<Literal>]
      let code_namespace = "code.namespace"

      /// The source code file name that identifies the code unit as uniquely as possible (preferably an absolute file path).
      ///
      /// ValueType: string
      ///
      /// Examples: /usr/local/MyApplication/content_root/app/index.php
      ///
      /// Required: No
      [<Literal>]
      let code_filepath = "code.filepath"

      /// The line number in code.filepath best representing the operation. It SHOULD point within the code unit named in code.function.
      ///
      /// ValueType: string
      ///
      /// Examples: 42
      ///
      /// Required: No
      [<Literal>]
      let code_lineno = "code.lineno"

    module Exceptions =


      [<Literal>]
      let exception_ = "exception"

      /// The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it.
      ///
      /// ValueType: string
      ///
      /// Examples: java.net.ConnectException; OSError
      ///
      /// Required: No
      [<Literal>]
      let exception_type = "exception.type"

      /// The exception message.
      ///
      /// ValueType: string
      ///
      /// Examples: Division by zero; Can't convert 'int' object to str implicitly
      ///
      /// Required: No
      [<Literal>]
      let exception_message = "exception.message"

      /// A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.
      ///
      /// ValueType: string
      ///
      /// Examples: Exception in thread "main" java.lang.RuntimeException: Test exception\n at com.example.GenerateTrace.methodB(GenerateTrace.java:13)\n at com.example.GenerateTrace.methodA(GenerateTrace.java:9)\n at com.example.GenerateTrace.main(GenerateTrace.java:5)
      ///
      /// Required: No
      [<Literal>]
      let exception_stacktrace = "exception.stacktrace"

      /// SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span.
      ///
      /// An exception is considered to have escaped (or left) the scope of a span, if that span is ended while the exception is still logically "in flight". This may be actually "in flight" in some languages (e.g. if the exception is passed to a Context manager's __exit__ method in Python) but will usually be caught at the point of recording the exception in most languages.
      ///
      /// It is usually not possible to determine at the point where an exception is thrown whether it will escape the scope of a span. However, it is trivial to know that an exception will escape, if one checks for an active exception just before ending the span, as done in the example above.
      ///
      /// It follows that an exception may still escape the scope of the span even if the exception.escaped attribute was not set or set to false, since the event might have been recorded at a time where it was not clear whether the exception will escape.
      ///
      /// ValueType: boolean
      ///
      /// Required: No
      [<Literal>]
      let exception_escaped = "exception.escaped"

module private SemanticHelpers =
  let inline createSourceCodeTags (filePath: string) (codeLine: int) (name_space: string) (functionName: string) =
    seq {
      SemanticConventions.General.SourceCode.code_filepath, box filePath
      SemanticConventions.General.SourceCode.code_lineno, box codeLine
      SemanticConventions.General.SourceCode.code_namespace, box name_space
      SemanticConventions.General.SourceCode.code_function, box functionName
    }

[<Extension>]
type ActivityExtensions =

  /// <summary>
  /// Add or update the Activity baggage with the input key and value.
  ///
  /// If the input value is null
  ///     - if the collection has any baggage with the same key, then this baggage will get removed from the collection.
  ///     - otherwise, nothing will happen and the collection will not change.
  ///
  /// If the input value is not null
  ///     - if the collection has any baggage with the same key, then the value mapped to this key will get updated with the new input value.
  ///     - otherwise, the key and value will get added as a new baggage to the collection.
  ///
  ///
  /// https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.activity.setbaggage?view=net-6.0
  /// </summary>
  /// <param name="span">The activity to add the baggage to</param>
  /// <param name="key">The baggage key name</param>
  /// <param name="value">The baggage value mapped to the input key</param>
  /// <returns><see langword="this" /> for convenient chaining.</returns>
  [<Extension>]
  static member inline SetBaggageSafe(span: Activity, key: string, value: string) =
    if not (isNull span) then
      span.AddBaggage(key, value)
    else
      span

  /// <summary>
  /// Add <see cref="ActivityEvent" /> object to the <see cref="Events" /> list.
  ///
  /// https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.activity.addevent?view=net-6.0
  /// </summary>
  /// <param name="span">The activity to add the baggage to</param>
  /// <param name="e"> object of <see cref="ActivityEvent"/> to add to the attached events list.</param>
  /// <returns><see langword="this" /> for convenient chaining.</returns>
  [<Extension>]
  static member inline AddEventSafe(span: Activity, e: ActivityEvent) =
    if Funcs.isNotNull span then span.AddEvent(e) else span

  /// <summary>
  /// Add or update the Activity tag with the input key and value.
  ///
  /// If the input value is null
  ///     - if the collection has any tag with the same key, then this tag will get removed from the collection.
  ///     - otherwise, nothing will happen and the collection will not change.
  ///
  /// If the input value is not null
  ///     - if the collection has any tag with the same key, then the value mapped to this key will get updated with the new input value.
  ///     - otherwise, the key and value will get added as a new tag to the collection.
  /// </summary>
  /// <param name="span">The activity to add the baggage to</param>
  /// <param name="key">The tag key name</param>
  /// <param name="value">The tag value mapped to the input key</param>
  /// <returns><see langword="this" /> for convenient chaining.</returns>
  [<Extension>]
  static member inline SetTagSafe(span: Activity, key, value: obj) =
    if Funcs.isNotNull span then
      span.SetTag(key, value)
    else
      span

  [<Extension>]
  static member inline SetStatusErrorSafe(span: Activity, description: string) =
    span.SetTagSafe("otel.status_code", "ERROR").SetTagSafe("otel.status_description", description)

  [<Extension>]
  static member inline SetSourceCodeFilePath(span: Activity, value: string) =
    span.SetTagSafe(SemanticConventions.General.SourceCode.code_filepath, value)

  [<Extension>]
  static member inline SetSourceCodeLineNumber(span: Activity, value: int) =
    span.SetTagSafe(SemanticConventions.General.SourceCode.code_lineno, value)

  [<Extension>]
  static member inline SetSourceCodeNamespace(span: Activity, value: string) =
    span.SetTagSafe(SemanticConventions.General.SourceCode.code_namespace, value)

  [<Extension>]
  static member inline SetSourceCodeFunction(span: Activity, value: string) =
    span.SetTagSafe(SemanticConventions.General.SourceCode.code_function, value)

  [<Extension>]
  static member inline SetNetworkNetTransport
    (span: Activity, value: string<SemanticConventions.General.Network.net_transport_values>)
    =
    span.SetTagSafe(SemanticConventions.General.Network.net_transport, UMX.untag value)

  [<Extension>]
  static member inline SetNetworkNetHostConnectionType
    (span: Activity, value: string<SemanticConventions.General.Network.net_host_connection_type_values>)
    =
    span.SetTagSafe(SemanticConventions.General.Network.net_host_connection_type, UMX.untag value)

  [<Extension>]
  static member inline SetNetworkNetHostConnectionSubType
    (span: Activity, value: string<SemanticConventions.General.Network.net_host_connection_subtype_values>)
    =
    span.SetTagSafe(SemanticConventions.General.Network.net_host_connection_subtype, UMX.untag value)


  /// <summary>https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/exceptions.md#semantic-conventions-for-exceptions</summary>
  /// <param name="span">The span to add the error information to</param>
  /// <param name="errorMessage">The exception message.</param>
  /// <param name="errorType">The type of the exception (its fully-qualified class name, if applicable). The dynamic type of the exception should be preferred over the static type in languages that support it.</param>
  /// <param name="stacktrace">A stacktrace as a string in the natural representation for the language runtime. The representation is to be determined and documented by each language SIG.</param>
  /// <param name="escaped">SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span. </param>
  [<Extension>]
  static member inline RecordError
    (span: Activity, errorMessage: string, errorType: string, ?stacktrace: string, ?escaped: bool)
    =
    if Funcs.isNotNull span then
      let escaped = defaultArg escaped false

      let tags =
        ActivityTagsCollection(
          seq {
            yield KeyValuePair(SemanticConventions.General.Exceptions.exception_escaped, box escaped)
            yield KeyValuePair(SemanticConventions.General.Exceptions.exception_type, box errorType)

            if Option.isSome stacktrace then
              yield KeyValuePair(SemanticConventions.General.Exceptions.exception_stacktrace, box stacktrace.Value)

            yield KeyValuePair(SemanticConventions.General.Exceptions.exception_message, box errorMessage)
          }
        )

      ActivityEvent(SemanticConventions.General.Exceptions.exception_, tags = tags)
      |> span.AddEvent
    else
      span

  /// <summary>https://github.com/open-telemetry/opentelemetry-specification/blob/main/specification/trace/semantic_conventions/exceptions.md#semantic-conventions-for-exceptions</summary>
  /// <param name="span">The span to add the error information to</param>
  /// <param name="e">The exception message.</param>
  /// <param name="escaped">SHOULD be set to true if the exception event is recorded at a point where it is known that the exception is escaping the scope of the span. </param>
  [<Extension>]
  static member inline RecordExceptions(span: Activity, e: exn, ?escaped: bool) =
    if Funcs.isNotNull span then
      let escaped = defaultArg escaped false
      let exceptionType = e.GetType().Name
      let exceptionStackTrace = e.ToString()
      let exceptionMessage = e.Message

      let tags =
        ActivityTagsCollection(
          seq {
            yield KeyValuePair(SemanticConventions.General.Exceptions.exception_escaped, box escaped)
            yield KeyValuePair(SemanticConventions.General.Exceptions.exception_type, box exceptionType)
            yield KeyValuePair(SemanticConventions.General.Exceptions.exception_stacktrace, box exceptionStackTrace)
            if not <| String.IsNullOrEmpty(exceptionMessage) then
              yield KeyValuePair(SemanticConventions.General.Exceptions.exception_message, box exceptionMessage) }
        )

      ActivityEvent(SemanticConventions.General.Exceptions.exception_, tags = tags)
      |> span.AddEvent
    else
      span

[<Extension>]
type ActivitySourceExtensions =

  /// <summary>Creates and starts a new System.Diagnostics.Activity object if there is any listener to the Activity events, returns null otherwise.</summary>
  /// <param name="tracer">Provides APIs to create and start System.Diagnostics.Activity objects.</param>
  /// <param name="name">The operation name of the Activity.</param>
  /// <param name="name_space">The namespace where this code is located.</param>
  /// <param name="activityKind">The System.Diagnostics.ActivityKind</param>
  /// <param name="parentContext">The parent System.Diagnostics.ActivityContext object to initialize the created Activity object with.</param>
  /// <param name="tags">The optional tags list to initialize the created Activity object with.</param>
  /// <param name="links">The optional System.Diagnostics.ActivityLink list to initialize the created Activity object with.</param>
  /// <param name="startTime">The optional start timestamp to set on the created Activity object.</param>
  /// <param name="memberName">Uses CallerMemberName, should not be set unless you know what you're doing.</param>
  /// <param name="path">Uses CallerFilePath, should not be set unless you know what you're doing.</param>
  /// <param name="line">Uses CallerLineNumberAttribute, should not be set unless you know what you're doing.</param>
  /// <returns>The created System.Diagnostics.Activity object or null if there is no any listener.</returns>
  [<Extension>]
  static member inline StartActivityExt
    (
      tracer: ActivitySource,
      ?name: string,
      ?name_space: string,
      ?activityKind: ActivityKind,
      ?parentContext: ActivityContext,
      ?tags: IEnumerable<string * obj>,
      ?links: IEnumerable<ActivityLink>,
      ?startTime: DateTimeOffset,
      [<CallerMemberName>] ?memberName: string,
      [<CallerFilePath>] ?path: string,
      [<CallerLineNumber>] ?line: int
    ) =

    let name_space =
      name_space
      |> Option.defaultWith (fun () ->
        Reflection.MethodBase.GetCurrentMethod().DeclaringType.FullName.Split("+")
        |> Seq.tryHead
        |> Option.defaultValue "")


    let kind = defaultArg activityKind ActivityKind.Internal

    let tags =
      seq {
        yield! SemanticHelpers.createSourceCodeTags path.Value line.Value name_space memberName.Value

        match tags with
        | Some t -> yield! t
        | None -> ()
      }
      |> Seq.map KeyValuePair

    let name = name |> Option.defaultWith (fun () -> $"{name_space}.{memberName.Value}")

    let span =
      tracer.StartActivity(
        name = name,
        kind = kind,
        ?parentContext = parentContext,
        tags = tags,
        ?links = links,
        ?startTime = startTime
      )

    span

  /// <summary>This should be used by methods in classes. Creates and starts a new System.Diagnostics.Activity object if there is any listener to the Activity events, returns null otherwise.</summary>
  /// <param name="tracer">Provides APIs to create and start System.Diagnostics.Activity objects.</param>
  /// <param name="ty">The type where the trace is located.</param>
  /// <param name="activityKind">The System.Diagnostics.ActivityKind</param>
  /// <param name="parentContext">The parent System.Diagnostics.ActivityContext object to initialize the created Activity object with.</param>
  /// <param name="tags">The optional tags list to initialize the created Activity object with.</param>
  /// <param name="links">The optional System.Diagnostics.ActivityLink list to initialize the created Activity object with.</param>
  /// <param name="startTime">The optional start timestamp to set on the created Activity object.</param>
  /// <param name="memberName">Uses CallerMemberName, should not be set unless you know what you're doing.</param>
  /// <param name="path">Uses CallerFilePath, should not be set unless you know what you're doing.</param>
  /// <param name="line">Uses CallerLineNumberAttribute, should not be set unless you know what you're doing.</param>
  /// <returns>The created System.Diagnostics.Activity object or null if there is no any listener.</returns>
  [<Extension>]
  static member inline StartActivityForType
    (
      tracer: ActivitySource,
      ty: Type,
      ?activityKind: ActivityKind,
      ?parentContext: ActivityContext,
      ?tags: IEnumerable<string * obj>,
      ?links: IEnumerable<ActivityLink>,
      ?startTime: DateTimeOffset,
      [<CallerMemberName>] ?memberName: string,
      [<CallerFilePath>] ?path: string,
      [<CallerLineNumber>] ?line: int
    ) =
    let name_space = ty.FullName
    let name = $"{name_space}.{memberName.Value}"

    tracer.StartActivityExt(
      name,
      name_space = name_space,
      ?activityKind = activityKind,
      ?parentContext = parentContext,
      ?tags = tags,
      ?links = links,
      ?startTime = startTime,
      ?memberName = memberName,
      ?path = path,
      ?line = line
    )


  /// <summary>This should be used by methods in classes. Creates and starts a new System.Diagnostics.Activity object if there is any listener to the Activity events, returns null otherwise.</summary>
  /// <param name="tracer">Provides APIs to create and start System.Diagnostics.Activity objects.</param>
  /// <param name="activityKind">The System.Diagnostics.ActivityKind</param>
  /// <param name="parentContext">The parent System.Diagnostics.ActivityContext object to initialize the created Activity object with.</param>
  /// <param name="tags">The optional tags list to initialize the created Activity object with.</param>
  /// <param name="links">The optional System.Diagnostics.ActivityLink list to initialize the created Activity object with.</param>
  /// <param name="startTime">The optional start timestamp to set on the created Activity object.</param>
  /// <param name="memberName">Uses CallerMemberName, should not be set unless you know what you're doing.</param>
  /// <param name="path">Uses CallerFilePath, should not be set unless you know what you're doing.</param>
  /// <param name="line">Uses CallerLineNumberAttribute, should not be set unless you know what you're doing.</param>
  /// <typeparam name="'typAr">The type where the trace is located.</typeparam>
  /// <returns>The created System.Diagnostics.Activity object or null if there is no any listener.</returns>
  [<Extension>]
  static member inline StartActivityForType<'typAr>
    (
      tracer: ActivitySource,
      ?activityKind: ActivityKind,
      ?parentContext: ActivityContext,
      ?tags: IEnumerable<string * obj>,
      ?links: IEnumerable<ActivityLink>,
      ?startTime: DateTimeOffset,
      [<CallerMemberName>] ?memberName: string,
      [<CallerFilePath>] ?path: string,
      [<CallerLineNumber>] ?line: int
    ) =
    let ty = typeof<'typAr>

    tracer.StartActivityForType(
      ty,
      ?activityKind = activityKind,
      ?parentContext = parentContext,
      ?tags = tags,
      ?links = links,
      ?startTime = startTime,
      ?memberName = memberName,
      ?path = path,
      ?line = line
    )

  /// <summary>This should be used by functions in modules. Creates and starts a new System.Diagnostics.Activity object if there is any listener to the Activity events, returns null otherwise.</summary>
  /// <param name="tracer">Provides APIs to create and start System.Diagnostics.Activity objects.</param>
  /// <param name="activityKind">The System.Diagnostics.ActivityKind</param>
  /// <param name="parentContext">The parent System.Diagnostics.ActivityContext object to initialize the created Activity object with.</param>
  /// <param name="tags">The optional tags list to initialize the created Activity object with.</param>
  /// <param name="links">The optional System.Diagnostics.ActivityLink list to initialize the created Activity object with.</param>
  /// <param name="startTime">The optional start timestamp to set on the created Activity object.</param>
  /// <param name="memberName">Uses CallerMemberName, should not be set unless you know what you're doing.</param>
  /// <param name="path">Uses CallerFilePath, should not be set unless you know what you're doing.</param>
  /// <param name="line">Uses CallerLineNumberAttribute, should not be set unless you know what you're doing.</param>
  /// <returns>The created System.Diagnostics.Activity object or null if there is no any listener.</returns>
  [<Extension>]
  static member inline StartActivityForFunc
    (
      tracer: ActivitySource,
      ?activityKind: ActivityKind,
      ?parentContext: ActivityContext,
      ?tags: IEnumerable<string * obj>,
      ?links: IEnumerable<ActivityLink>,
      ?startTime: DateTimeOffset,
      [<CallerMemberName>] ?memberName: string,
      [<CallerFilePath>] ?path: string,
      [<CallerLineNumber>] ?line: int
    ) =

    tracer.StartActivityExt(
      ?name = None,
      ?name_space = None,
      ?activityKind = activityKind,
      ?parentContext = parentContext,
      ?tags = tags,
      ?links = links,
      ?startTime = startTime,
      ?memberName = memberName,
      ?path = path,
      ?line = line
    )

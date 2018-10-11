module FsAutoComplete.Analyzers

open System
open System.IO
open System.Reflection
open FSharp.Analyzers.SDK

let attributeName = "AnalyzerAttribute"

let isAnalyzer (mi: MemberInfo) =
    mi.GetCustomAttributes true
    |> Seq.exists (fun n -> n.GetType().Name = attributeName)

let analyzerFromMember (mi: MemberInfo) : Analyzer option =
    let inline unboxAnalyzer v =
      if isNull v then
        failwith "Analyzer is null"
      else unbox v
    let getAnalyzerFromMemberInfo mi =
      match box mi with
      | :? FieldInfo as m ->
        if m.FieldType = typeof<Analyzer> then Some(m.GetValue(null) |> unboxAnalyzer)
        else None
      | :? MethodInfo as m ->
        if m.ReturnType = typeof<Analyzer>
          then Some(m.Invoke(null, null) |> unboxAnalyzer)
        elif m.ReturnType.FullName.StartsWith "Microsoft.FSharp.Collections.FSharpList`1[[FSharp.Analyzers.SDK.Message" then
          try
            let x : Analyzer = fun ctx ->
              try
                m.Invoke(null, [|ctx|]) |> unbox
              with
              | ex -> []
            Some x
          with
          | ex -> None
        else None
      | :? PropertyInfo as m ->
        if m.PropertyType = typeof<Analyzer> then Some(m.GetValue(null, null) |> unboxAnalyzer)
        else None
      | _ -> None
    if isAnalyzer mi then getAnalyzerFromMemberInfo mi else None

let analyzersFromType (t: Type) =
    let asMembers x = Seq.map (fun m -> m :> MemberInfo) x
    let bindingFlags = BindingFlags.Public ||| BindingFlags.Static

    let members =
      [ t.GetTypeInfo().GetMethods bindingFlags |> asMembers
        t.GetTypeInfo().GetProperties bindingFlags |> asMembers
        t.GetTypeInfo().GetFields bindingFlags |> asMembers ]
      |> Seq.collect id
    members
    |> Seq.choose analyzerFromMember
    |> Seq.toList

let loadAnalyzers (dir: FilePath): Analyzer list =
    if Directory.Exists dir then
      let dlls =
          Directory.GetFiles(dir, "*Analyzer*.dll", SearchOption.AllDirectories)
          |> Array.choose (fun n ->
            try
              Some (Assembly.LoadFile n)
            with
            | _ -> None)
      dlls
      |> Array.collect (fun a -> a.GetExportedTypes())
      |> Seq.collect (analyzersFromType)
      |> Seq.toList
    else
      []
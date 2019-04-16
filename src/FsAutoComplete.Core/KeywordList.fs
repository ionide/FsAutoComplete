namespace FsAutoComplete

module internal KeywordList =

    let private keywordDescriptions =
        FSharp.Compiler.SourceCodeServices.Keywords.KeywordsWithDescription
        |> Map.ofList

    let tryGetKeywordDescription (keyword: string) =
        keywordDescriptions
        |> Map.tryFind keyword

    let allKeywords : string list =
        FSharp.Compiler.SourceCodeServices.Keywords.KeywordsWithDescription
        |> List.map fst

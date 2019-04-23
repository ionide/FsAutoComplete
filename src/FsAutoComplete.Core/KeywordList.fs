namespace FsAutoComplete

module KeywordList =

    let private keywordDescriptions =
        FSharp.Compiler.SourceCodeServices.Keywords.KeywordsWithDescription
        |> Map.ofList

    let hashDirectives =
            ["r", "References an assembly"
             "load", "Reads a source file, compiles it, and runs it."
             "I", "Specifies an assembly search path in quotation marks."
             "light", "Enables or disables lightweight syntax, for compatibility with other versions of ML"
             "if", "Supports conditional compilation"
             "else", "Supports conditional compilation"
             "endif", "Supports conditional compilation"
             "nowarn", "Disables a compiler warning or warnings"
             "line", "Indicates the original source code line"]

    let tryGetKeywordDescription (keyword: string) =
        keywordDescriptions
        |> Map.tryFind keyword

    let tryGetHashDescription (hash: string) =
        hashDirectives
        |> List.tryFind (fun (h,_ ) -> h = hash)
        |> Option.map snd

    let allKeywords : string list =
        FSharp.Compiler.SourceCodeServices.Keywords.KeywordsWithDescription
        |> List.map fst

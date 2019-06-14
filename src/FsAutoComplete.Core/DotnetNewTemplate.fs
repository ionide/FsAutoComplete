namespace FsAutoComplete

module DotnetNewTemplate =

  type Template = {
    Name : string;
    ShortName : string;
    Language: TemplateLanguage list;
    Tags: string list
  }
  and TemplateLanguage = CSharp | FSharp | VB

  let installedTemplates () : Template list =
    [ { Name = "Console Application";
        ShortName = "console";
        Language = [ TemplateLanguage.CSharp; TemplateLanguage.FSharp; TemplateLanguage.VB ];
        Tags = ["Common"; "Console"] };

      { Name = "Class library";
        ShortName = "classlib";
        Language = [ TemplateLanguage.CSharp; TemplateLanguage.FSharp; TemplateLanguage.VB ];
        Tags = ["Common"; "Library"]  }; 
    ]
  
  let filterByName (filterstr : string) (installedTemplates : Template list) : bool =
    installedTemplates.Name.ToLower() = filterstr.ToLower() 
    
  let extractString (t : Template list) =
    [t.Name,t.ShortName]

  let dotnetnewlist (userInput : string) =
    installedTemplates
    |> List.contains (filterByName userInput)

    installedTemplates
    |> List.map (fun t -> t, extractString t)
    |> List.filter (fun strings -> strings |> List.exists (IsMatch(userInput)))
    |> List.map (fun (t,strings) -> t)
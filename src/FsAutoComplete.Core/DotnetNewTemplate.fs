namespace FsAutoComplete

module DotnetNewTemplate =
  type Template = {
    Name : string;
    ShortName : string;
    Language: TemplateLanguage list;
    Tags: string list
  } 
  and TemplateLanguage = CSharp | FSharp | VB
  and TemplateParameter = {
    Name : string;
    ShortName : string;
    ParameterType : TemplateParameterType;
    Description : string;
    DefaultValue : string
  }
  and TemplateParameterType =
  | Bool
  | String
  | Choice of string list

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
  
  let isMatch (filterstr: string) (x: string) =
    x.ToLower().Contains(filterstr.ToLower())

  let extractString (t : Template) =
    [t.Name; t.ShortName]

  let dotnetnewlist (userInput : string) =
    installedTemplates ()
    |> List.map (fun t -> t, extractString t) // extract keywords from the template
    |> List.filter (fun (t, strings) ->  strings |> List.exists (isMatch userInput)) // check if a keyword match the user string
    |> List.map (fun (t,strings) -> t)  // return the template

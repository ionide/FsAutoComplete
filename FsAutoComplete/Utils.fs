namespace FsAutoComplete

open System.IO

module Utils =
  
  let isAScript fileName =
      let ext = Path.GetExtension fileName
      [".fsx";".fsscript";".sketchfs"] |> List.exists ((=) ext)

  let runningOnMono = 
    try System.Type.GetType("Mono.Runtime") <> null
    with _ -> false  


open System.IO

let fsw = new FileSystemWatcher()
fsw.Path <- __SOURCE_DIRECTORY__ + "/FsAutoComplete"
//fsw.Filter <- Path.GetFileName file
fsw.Filter <- "*.fsproj"
fsw.Changed.Add(fun x -> printfn "Project file '%s' changed in '%A' way" x.Name x.ChangeType)
fsw.EnableRaisingEvents <- true

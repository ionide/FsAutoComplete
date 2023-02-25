namespace FsAutoComplete.Core

module ProjectLoader =
  [<Literal>]
  let ProduceReferenceAssembly = "ProduceReferenceAssembly"

  let globalProperties =
    Map.ofList
      [
        // For tooling we don't want to use Reference Assemblies as this doesn't play well with type checking across projects
        ProduceReferenceAssembly, "false" ]

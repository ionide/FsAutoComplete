module TypeHierarchyExample

type IAnimal =
    abstract Sound: unit -> string

type Animal(name: string) =
    interface IAnimal with
        member _.Sound() = "..."
    member _.Name = name

type Dog(name: string) =
    inherit Animal(name)
    override this.ToString() = sprintf "Dog: %s" this.Name

type Cat(name: string) =
    inherit Animal(name)
    override this.ToString() = sprintf "Cat: %s" this.Name

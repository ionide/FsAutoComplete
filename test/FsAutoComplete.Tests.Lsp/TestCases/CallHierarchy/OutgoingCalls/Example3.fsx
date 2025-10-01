module OutgoingExample3 =

    // Cross-file reference example
    open System.IO
    open System.Collections.Generic

    let processData () =
        // System function calls
        let path = Path.Combine("temp", "file.txt")
        let exists = File.Exists(path)
        
        // Collection operations
        let list = List<int>()
        list.Add(1)
        list.Add(2)
        
        // Higher-order function calls
        let numbers = [1; 2; 3; 4; 5]
        let doubled = List.map (fun x -> x * 2) numbers
        let filtered = List.filter (fun x -> x > 5) doubled
        
        filtered

    ignore processData
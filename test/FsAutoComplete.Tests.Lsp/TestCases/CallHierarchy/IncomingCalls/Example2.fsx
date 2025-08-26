// Multiple levels of nesting and cross-function calls
module Example2 =
    
    let helper x = x + 1
    
    let processData data =
        data |> List.map helper
    
    let validateInput input =
        if input > 0 then
            processData [input]
        else
            []
    
    let mainFunction value =
        let result = validateInput value
        result |> List.sum
        
    // Direct call to helper
    let directCall = helper 42
    
    // Nested module scenario
    module Nested =
        let nestedHelper () = helper 100
        
    let callNestedFunction = Nested.nestedHelper
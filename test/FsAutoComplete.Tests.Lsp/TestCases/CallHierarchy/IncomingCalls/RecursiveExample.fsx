// Recursive function calls and mutual recursion
module RecursiveExample =
    
    let rec factorial n =
        if n <= 1 then 
            1 
        else 
            n * factorial (n - 1)
    
    let rec isEven n =
        if n = 0 then true
        else isOdd (n - 1)
    
    and isOdd n =
        if n = 0 then false
        else isEven (n - 1)
    
    // Call to recursive functions
    let testFactorial = factorial 5
    let testEven = isEven 4
    let testOdd = isOdd 3
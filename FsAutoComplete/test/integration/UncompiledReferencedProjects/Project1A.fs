
type C() = 
    static member M(arg1: int, arg2: int, ?arg3 : int) = arg1 + arg2 + defaultArg arg3 4
let x1 = C.M(arg1 = 3, arg2 = 4, arg3 = 5)
let x2 = C.M(arg1 = 3, arg2 = 4, ?arg3 = Some 5)

module Definitions

let sample_value = 123

type A = {x: int; y: int}

let value_with_type = {x = 123; y = 456}

type IInterface =
    abstract member C : int -> int
    abstract member D : int -> int
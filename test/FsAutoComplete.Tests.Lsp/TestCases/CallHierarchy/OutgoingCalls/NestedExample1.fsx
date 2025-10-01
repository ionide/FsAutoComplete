// Deeply nested call hierarchy example
// Level 1 -> Level 2 -> Level 3 -> Level 4

let level4Function x = x * 2

let level3Function x =
  let result = level4Function x
  result + 1

let level2Function x =
  let intermediate = level3Function x
  intermediate * 3

let level1Function x =
  let first = level2Function x
  let second = level2Function (x + 1)
  first + second

// Additional branching calls
let anotherLevel3 x = level4Function x + level4Function (x * 2)

let complexLevel2 x =
  let a = level3Function x
  let b = anotherLevel3 x
  a + b

let entryPoint x =
  let primary = level1Function x
  let secondary = complexLevel2 x
  primary + secondary

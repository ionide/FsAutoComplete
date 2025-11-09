// Simple deeply nested hierarchy for testing deep navigation
let level4Action x = x + 1

let level3Helper input = level4Action input

let level2Process data = level3Helper data

let level1Main value = level2Process value

let topLevelEntry x = level1Main x

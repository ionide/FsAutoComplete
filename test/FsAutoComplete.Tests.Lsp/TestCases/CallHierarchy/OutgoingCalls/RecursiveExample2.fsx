module RecursiveExample2 =

  // Tree-like call structure
  type TreeNode = { Value: int; Children: TreeNode list }

  let rec sumTree node =
    let childrenSum = List.fold (+) 0 (List.map sumTree node.Children)
    node.Value + childrenSum

  let rec findMax node =
    if List.isEmpty node.Children then
      node.Value
    else
      let childMax = List.max (List.map findMax node.Children)
      max node.Value childMax

  // Multi-level call chain with branching
  let rec processData input = validateInput input |> transformData |> computeResult

  and validateInput data = if data > 0 then data else failwith "Invalid input"

  and transformData data = helper1 data + helper2 data

  and computeResult data = finalStep (intermediateStep data)

  and helper1 x = x * 2
  and helper2 x = x + 10
  and intermediateStep x = x / 2
  and finalStep x = x - 1

  // Function that creates and processes a tree
  let createAndProcessTree () =
    let tree =
      { Value = 1
        Children =
          [ { Value = 2; Children = [] }
            { Value = 3
              Children = [ { Value = 4; Children = [] } ] } ] }

    let sum = sumTree tree
    let maxVal = findMax tree
    processData sum + processData maxVal

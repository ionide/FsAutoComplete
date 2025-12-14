// Complex nested hierarchy with data processing pipeline
// Multiple levels with various call patterns

module DataProcessing =
  let formatData value = sprintf "Data: %A" value

  let validateData value = if value > 0 then Some value else None

  let transformData value =
    let formatted = formatData value
    let validated = validateData value

    match validated with
    | Some v -> v * 2
    | None -> 0

module BusinessLogic =
  open DataProcessing

  let processInput input =
    let transformed = transformData input
    transformed + 10

  let aggregateResults inputs = inputs |> List.map processInput |> List.sum

  let calculateMetrics inputs =
    let sum = aggregateResults inputs
    let count = List.length inputs
    if count > 0 then sum / count else 0

module Services =
  open BusinessLogic
  open DataProcessing

  let serviceOperation inputs =
    let metrics = calculateMetrics inputs
    let finalData = transformData metrics
    finalData

  let orchestrateWorkflow data =
    let processed = serviceOperation data
    let validated = validateData processed

    match validated with
    | Some result ->
      let formatted = formatData result
      Some formatted
    | None -> None

// Entry points that create deep call chains
let mainWorkflow data = Services.orchestrateWorkflow data

let parallelWorkflow data1 data2 =
  let result1 = Services.serviceOperation data1
  let result2 = Services.serviceOperation data2
  [ result1; result2 ]

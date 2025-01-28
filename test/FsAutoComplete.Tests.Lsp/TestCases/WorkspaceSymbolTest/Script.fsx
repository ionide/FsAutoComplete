type X = { Name: string }

module X =

  let getName x = x.Name

  module X =

    let doSideEffect() = ()

  module Y =

    let getName x = x.Name

  type Z = { Name: string }

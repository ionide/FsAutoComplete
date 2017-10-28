

module XA =
  let funky x = x + 1

let val99 = XA.funky 21

module CommandResponse =

  type ResponseMsg<'T> =
    {
      Kind: string
      Data: 'T
    }

    member x.f y = 1

///some random xml docs
let funct (x : CommandResponse.ResponseMsg<_>) = ()

let x = System.DateTime(123L)
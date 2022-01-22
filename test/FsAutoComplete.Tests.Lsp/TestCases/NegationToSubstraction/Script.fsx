let getListWithoutFirstAndLastElement list =
  let l = List.length list
  list[ 1 .. l -1 ]

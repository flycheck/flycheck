let test_member x tup =
  match tup with
  | (y, _) | (_, y) when y = x -> true
  | _ -> false;;

let is_even x =
    match x mod 2 with
    | 0 -> true
    | 1 | -1 -> false;;

undefined

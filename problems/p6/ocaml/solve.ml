let ( >> ) f g x = g (f x)

let ( **. ) = Stdlib.( ** )
let ( ** ) a b = (float_of_int a) **. (float_of_int b) |> int_of_float

let sum = List.fold_left ( + ) 0

let range n m =
  let rec loop x acc = if x <= m then loop (x + 1) (x :: acc) else List.rev acc in
  loop n []

let solve n =
  let ns = range 1 n in
  ((ns |> sum) ** 2) - (ns |> List.map (fun n -> n ** 2) |> sum)

let () = assert (solve 10 = 2640)
let () = assert (solve 100 = 25164150)

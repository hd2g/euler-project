let rec range n m = if n < m then n :: range (n + 1) m else []

let sum = List.fold_left ( + ) 0

let solve n =
  range 1 n
  |> List.filter (fun x -> x mod 3 = 0 || x mod 5  = 0)
  |> sum

let () = assert (solve 10 == 23)
let () = assert (solve 1000 == 233168)

let iota m n =
  let rec loop x acc = if x < m then loop (x + 1) (x :: acc) else List.rev acc in
  loop n []

let pythagoreans m =
  assert (2 <= m);
  iota m 1
  |> List.map (fun n -> (m * m - n * n, 2 * m * n, m * m + n * n))

let solve summary =
  iota summary 2 |> List.concat_map pythagoreans |> List.find (fun (a, b, c) -> a + b + c = summary)
  
let list_of_tuple = function
  | a, b, c -> [a; b; c]

let () =
  assert (solve 12 |> list_of_tuple |> List.fold_left ( * ) 1 = 60);
  assert (solve 1000 |> list_of_tuple |> List.fold_left ( * ) 1 = 31875000)

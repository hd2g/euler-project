let range n m =
  let rec go x acc = if x <= m then go (x + 1) (x :: acc) else List.rev acc in
  go n []

let terms_of_collatz n =
  let rec go n acc =
    if n = 1 then acc + 1
    else if n mod 2 = 0 then go (n / 2) (acc + 1)
    else go (3 * n + 1) (acc + 1)
  in
  go n 0

module List = struct
  let max_by f self =
    let rec go (calced, acc) = function
      | [] -> acc
      | x :: xs ->
         let calced' = f x in
         go (if calced' < calced then (calced, acc) else (calced', x)) xs
    in
    go (0, 0) self

  include List
end

let solve n = range 1 n |> List.max_by terms_of_collatz

let () = assert (solve 1000000 = 837799)

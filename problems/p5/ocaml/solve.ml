let rec gcd a b = if b = 0 then a else gcd b (a mod b)

let lcm a b = if a = 0 || b = 0 then 0 else (a * b) / (gcd a b)

let range n m =
  let rec loop x acc = if x <= m then loop (x + 1) (x :: acc) else List.rev acc in
  loop n []

let () = assert ((range 2 10 |> List.fold_left lcm 1) = 2520)
let () = assert ((range 2 20 |> List.fold_left lcm 1) = 232792560)

let peek_while pred sm =
  let rec loop acc =
    try
      let x = Stream.next sm in
      loop (if pred x then (x :: acc) else acc)
    with
    | _ -> List.rev acc
  in
  loop []

let take_while pred ls =
  let rec loop acc = function
    | [] -> List.rev acc
    | x :: xs ->
       if pred x then loop (x :: acc) xs
       else List.rev acc
  in
  loop [] ls

let sum_float = List.fold_left ( +. ) 0.

let sqrt_int n = int_of_float (sqrt (float_of_int n))

let fibs () =
  let a, b = ref 0., ref 1. in
  Stream.from
    (fun _ ->
      let ret = !a +. !b in
      a := !b;
      b := ret;
      Some ret)

let solve n =
  fibs ()
  |> peek_while (fun x -> x <= n)
  |> List.filter (fun x -> mod_float x 2. = 0.)
  |> sum_float

let () = assert (solve 4000000. = 4613732.)

let () = print_endline "ok"

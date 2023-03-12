let range (n : float) (m : float) : float list =
  let rec loop x acc =
    if x <= m then loop (x +. 1.) (x :: acc) else List.rev acc
  in
  loop n []

let rec factors (n : float) : float list =
  range 2. (sqrt n)
  |> List.find_opt (fun x -> mod_float n x = 0.)
  |> Option.fold
       ~none:[n]
       ~some:(fun x -> x :: factors (n /. x))

let () = assert (factors 13195. = [5.; 7.; 13.; 29.])
let () = assert (factors 600851475143. = [71.; 839.; 1471.; 6857.])

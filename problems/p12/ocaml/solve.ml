let range n m =
  let rec go x acc = if x <= m then go (x +. 1.) (x :: acc) else List.rev acc in
  go n []

let rec divisors n =
  range 2. (sqrt n)
  |> List.find_opt (fun i -> mod_float n i = 0.)
  |> Option.fold
       ~none:[n]
       ~some:(fun i  -> i :: (divisors (n /. i)))

module Triangles = struct
  let find (pred : float -> bool) : float =
    let rec go idx memo =
      let triangle = memo +. idx in
      if pred triangle then triangle
      else
        begin
          (* Format.printf "%.0f's triangle: %.0f\n" idx triangle; *)
          go (idx +. 1.) triangle
        end
    in
    go 0. 0.

  let findi (pred : float -> float -> bool) : (float * float) =
    let rec go idx memo =
      let triangle = memo +. idx in
      if pred triangle idx then (triangle, idx) else go (idx +. 1.) triangle
    in
    go 0. 0.
end

module List = struct
  let rec take_while (pred : 'a -> bool) = function
    | [] -> []
    | x :: xs -> if pred x then x :: take_while pred xs else []

  let rec drop_while (pred : 'a -> bool) = function
    | [] -> []
    | x :: xs as xss -> if pred x then drop_while pred xs else xss

  (* examples:
   * List.parted [1; 2; 2; 3; 3; 3] (* -> [[1]; [2; 2]; [3;3;3]] *)
   *)
  let rec parted = function
    | [] -> []
    | x :: _ as xss -> (take_while (fun a -> a = x) xss) :: parted (drop_while (fun a -> a = x) xss)

  include List
end

let count_of_divisors n =
  divisors n
  |> List.parted
  |> List.fold_left (fun acc ns -> acc * (List.length ns + 1)) 1

let solve threshould =
  Triangles.find
    (fun triangle ->
      threshould < count_of_divisors triangle)

let main () =
  assert (solve 5 = 28.);
  assert (solve 500 = 76576500.)

let () = main ()

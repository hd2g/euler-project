let ( >> ) f g x = g (f x)

let sqrt_int = float_of_int >> sqrt >> int_of_float

let range n m =
  let rec loop x acc = if x <= m then loop (x + 1) (x :: acc) else List.rev acc in
  loop n []

let is_prime n = if n = 2 || n = 3 then true else if n mod 2 = 0 then false else range 3 (sqrt_int n) |> List.exists (fun x -> n mod x = 0) |> not

let rec next_prime n =
  if n = 2 then 3
  else if n mod 2 = 0 then next_prime (n + 1)
  else if is_prime (n + 2) then n + 2
  else next_prime (n + 2)

let primes =
  let prime = ref 2 in
  Stream.from
    (fun _ ->
      let ret = !prime in
      prime := next_prime !prime;
      Some ret)

module List = struct
  let last = List.rev >> List.hd

  include List
end

let () = assert (primes |> Stream.npeek 6 |> List.last = 13)
let () = assert (primes |> Stream.npeek 10001 |> List.last = 104743)

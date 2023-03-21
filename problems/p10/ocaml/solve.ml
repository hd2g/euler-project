let ( >> ) f g x = g (f x)

module Sieve = struct
  type t = elm array
  and elm = Prime | Non_prime | Unknown

  let init limit =
    let self = Array.make limit Unknown in
    self.(0) <- Non_prime;
    self.(1) <- Non_prime;
    self

  let marked self =
    let rec mark i =
      let rec go i d =
        if i < Array.length self then
          (self.(i) <- Non_prime; go (i + d) d)
        else ()
      in
      if self.(i) = Unknown then
        (self.(i) <- Prime; go (i * 2) i)
      else ()
    in
    let rec go i d =
      if i < Array.length self then
        match self.(i) with
        | Prime | Unknown -> (mark i; go (i + 1) (d + 1))
        | _ -> go (i + 1) i
      else ()
    in
    go 2 1;
    self

  let to_list =
    Array.mapi (fun i elm -> match elm with Prime -> Some (i) | _ -> None)
    >> Array.to_list
    >> List.filter_map (function Some (i) -> Some (float_of_int i) | None -> None)

  let sieved limit = init limit |> marked |> to_list
end

let solve limit = Sieve.sieved limit |> List.fold_left ( +. ) 0.

let () = assert (solve 10 = 17.)
let () = assert (solve 2000000 = 142913828922.)

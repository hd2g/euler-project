let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)

let last = List.rev >> List.hd

module String = struct
  let rev = String.to_seq >> List.of_seq >> List.rev >> List.to_seq >> String.of_seq

  include String
end

let split s =
  let len = String.length s in
  let md = len / 2 in
  let d = if len mod 2 <> 0 then 1 else 0 in
  String.sub s 0 md, String.sub s (md + d) (len - md - d)

let () =
  assert (split "9009" = ("90", "09"));
  assert (split "90109" = ("90", "09"));
  assert true

let is_palindromic_opt n =
  let lhs, rhs = split (string_of_int n) in
  if lhs = String.rev rhs then Some (n) else None

let () =
  assert (is_palindromic_opt 9009 = Some (9009));
  assert (is_palindromic_opt 9019 = None);
  assert true

let range n m : int list =
  let rec loop x acc =
    if x <= m then loop (x + 1) (x :: acc) else List.rev acc
  in
  loop n []

let ( **. ) = Stdlib.( ** )
let ( ** ) a b = Stdlib.( ** ) (float_of_int a) (float_of_int b) |> int_of_float

let solve digit : int list =
  let xs = range (10 ** (digit - 1)) (10 ** digit - 1) in
  xs |> List.map (fun x -> xs |> List.filter_map (fun y -> is_palindromic_opt (x * y))) |> List.flatten

let () = assert (solve 2 |> last = 9009)
let () = assert (solve 3 |> last = 580085)

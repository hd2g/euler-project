let read_lines pathname =
  let ic = open_in pathname in
  let rec loop () =
    try
      let line = input_line ic in
      line :: loop ()
    with
    | End_of_file ->
        close_in ic;
        []
  in loop ()

let () = print_string "reading dat..."
let dat = read_lines "../dat" |> String.concat ""
let () = print_endline "done"

module List = struct
  let rec take n = function
    | [] -> []
    | x :: xs -> if 0 < n then x :: take (n - 1) xs else []

  let rec drop n = function
    | [] -> []
    | _ :: xs as xss -> if 0 < n then drop (n - 1) xs else xss

  include List
end

let max ns =
  let rec loop n = function
    | [] -> n
    | x :: xs ->
        let n' = if n < x then x else n in
        loop n' xs
  in loop 0. ns

let rec solve ss digit =
  if String.length ss < digit then []
  else
    let ss' =
      ss
      |> String.to_seq
      |> List.of_seq
      |> List.map (String.make 1)
    in
    let calced =
      ss'
      |> List.take digit
      |> List.map float_of_string
      |> List.fold_left ( *. ) 1.
    in
    let tl = 
      ss'
      |> List.drop 1
      |> List.map (fun s -> s.[0])
      |> List.to_seq
      |> String.of_seq
    in
    calced :: solve tl digit

let () = assert ((solve dat 4 |> max) = 5832.)
(* let () = assert ((solve dat 13 |> max) = 2091059712) *)

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
let dat = read_lines "../dat"
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

let max a b = if a < b then b else a

let chars_of_string s =
  s
  |> String.to_seq
  |> List.of_seq
  |> List.map (String.make 1)

let string_of_chars cs =
  cs
  |> List.map (fun s -> s.[0])
  |> List.to_seq
  |> String.of_seq

let rec solve' (ss : string) (digit : int) : float list =
  if String.length ss < digit then []
  else
    let ss' = chars_of_string ss in
    let calced =
      ss'
      |> List.take digit
      |> List.map float_of_string
      |> List.fold_left ( *. ) 1.
    in
    let tl = 
      ss'
      |> List.drop 1
      |> string_of_chars
    in
    calced :: solve' tl digit

let solve (lines : string list) (digit : int) : float list =
  List.fold_left
    (fun (acc : float list) (line : string) -> List.append acc (solve' line digit))
    []
    lines

let () = assert ((solve dat 4 |> List.fold_left max 0.) = 5832.)
let () = assert ((solve dat 13 |> List.fold_left max 0.) = 5377010688.)

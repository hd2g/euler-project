let read_lines filename =
  let ic = open_in filename in
  let rec go acc =
    try
      let line = input_line ic in
      go (line :: acc)
    with
    | _ -> (close_in ic; List.rev acc)
  in
  go []

let dat = read_lines "../dat"
  |> Array.of_list
  |> Array.map
      (fun line ->
        line
        |> String.split_on_char ' '
        |> List.map int_of_string
        |> Array.of_list)

let ( >> ) f g x = g (f x)

module Array = struct
  let lift (idx : int) (self : 'a array) : 'a option =
    if idx < 0 || Array.length self <= idx then None
    else Some (self.(idx))

  include Array
end

module Option = struct
  let join_map (f : 'a -> 'b option) (self : 'a option) : 'b option = assert false

  include Option
end

let range n m =
  let rec go x acc = if x <= m then go (x + 1) (x :: acc) else List.rev acc in
  go n []

module Darray = struct
  type 'a t = 'a array array

  let lift (y : int) (x : int) (self : 'a t) : 'a option =
    Array.lift y self
    |> Option.map (Array.lift x)
    |> Option.join

  let height (self : 'a t) : int = Array.length self

  let width (self : 'a t) : int =
    if Array.length self = 0 then 0
    else Array.length self.(0)
end

let ident x = x

let list_around size y x src =
  let xs = range x (x + size - 1) in
  let ys = range y (y + size - 1) in
  let rights = xs |> List.filter_map (fun x -> Darray.lift y x src) in
  let unders = ys |> List.filter_map (fun y -> Darray.lift y x src) in
  let diagonals = List.map2 (fun y x -> Darray.lift y x src) ys xs |> List.filter_map ident in
  [ rights; unders; diagonals ]

let product = List.fold_left ( * ) 1

let max = 
  let max' a b = if a < b then b else a in
  List.fold_left max' 0

let solve src size =
  let hs = range 0 (Darray.height src - 1) in
  let ws = range 0 (Darray.width src - 1) in
  List.concat_map (fun y -> List.concat_map (fun x -> list_around size y x src) ws) hs
  |> List.map (product)
  |> max

let () = assert (solve dat 4 = 51267216)

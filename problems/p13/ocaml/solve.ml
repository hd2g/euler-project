let read_lines pathname =
  let ic = open_in pathname in
  let rec go acc =
    try
      let line = input_line ic in
      go (line :: acc)
    with
    | End_of_file -> (close_in ic; List.rev acc)
  in
  go []

let ( >> ) f g x = g (f x)

let dat : int array array =
  read_lines "../dat"
  |> Array.of_list
  |> Array.map
       (fun (line : string) ->
         line
         |> String.to_seq
         |> List.of_seq
         |> List.rev
         |> Array.of_list
         |> Array.map (String.make 1 >> int_of_string))

let max a b = if a < b then b else a

module Array = struct
  let zip (xs : 'a array) (ys : 'b array) : ('a * 'b) array = Array.map2 (fun x y -> x, y) xs ys

  include Array
end

let rec add xs ys =
  let xl = Array.length xs in
  let yl = Array.length ys in
  let size = max xl yl in
  let buf = Array.make (size + 1) 0 in
  let xss = if xl < size then Array.append xs (Array.make (size - xl) 0) else xs in
  let yss = if yl < size then Array.append ys (Array.make (size - yl) 0) else ys in
  let zss =
    Array.zip xss yss
    |> Array.mapi
         (fun i (x, y) ->
           let summary = x + y in
           if summary < 10 then summary
           else
             begin
               Array.set buf (i + 1) 1;
               summary - 10
             end)
  in
  if Array.for_all (( = ) 0) buf then zss
  else add zss buf

module List = struct
  let rec drop_while pred = function
    | [] -> []
    | x :: xs as self -> if pred x then drop_while pred xs else self

  let take n = List.filteri (fun i _ -> i < n)

  include List
end

let solve (src : int array array) = Array.fold_left add (Array.make (Array.length (Array.get src 0)) 0) src |> Array.to_list |> List.rev |> List.drop_while (( = ) 0)

let () = assert (solve dat |> List.take 10 = [5; 5; 3; 7; 3; 7; 6; 2; 3; 0])

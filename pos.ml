type t = int * int

let create x y = (x, y)

let x (x, _) = x

let y (_, y) = y

let to_string (x, y) =
  Printf.sprintf "(%d,%d)" x y

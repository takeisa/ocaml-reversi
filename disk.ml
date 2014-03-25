type t = Black | White

let reverse t =
  match t with
    Black -> White
  | White -> Black

open Core.Std

let eval board disk =
  let pos_list = Board.pos_list board disk in
  if List.is_empty pos_list then None
  else
    (List.iter pos_list ~f:(fun pos -> printf "%s" (Pos.to_string pos));
     print_endline "";
     let len = List.length pos_list in
     let pos = List.nth_exn pos_list (Random.int len) in
     Some pos
    )


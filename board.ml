open Core.Std

type t = (Pos.t * Cell.t) list

exception Runtime_error of string

let create () = []

let put t pos cell =
  List.Assoc.add t pos cell

let width = 8

let height = 8

(* val is_on_board : Pos.t -> int -> int -> bool *)
let is_on_board p w h =
  let (x, y) = (Pos.x p, Pos.y p) in
  x >= 0 && y >= 0 && x < w && y < h

(* val get : Board.t -> Pos.t -> Disk.t *)
let get t pos =
  if is_on_board pos width height then
    match List.Assoc.find t pos with
      Some cell -> cell
    | None -> Cell.None
  else Cell.Boarder

let pos x y = Pos.create x y

let from_string_list str_list =
  let remove_separator str =
      String.fold str ~init:(0, "") ~f:(fun (index, s) c ->
          let s' =
            if index mod 2 = 0 then s ^ (Char.to_string c)
            else s in
          (index + 1, s'))
  in
  let char_to_disk c =
    match c with
      'X' -> Some Disk.Black
    | 'O' -> Some Disk.White
    | _ -> None
  in
  let str_to_pos_list y str =
    String.fold str ~init:(0, []) ~f:(fun (x, pos_list) c ->
        match char_to_disk c with
          Some d -> (x + 1, ((pos x y), d) :: pos_list)
        | None -> (x + 1, pos_list))
  in
  let str_list_to_pos_list str_list =
    List.fold str_list ~init:(0, []) ~f:(fun (y, pos_list) str ->
        let (_, str) = remove_separator str in
        let (_, pos_list') = str_to_pos_list y str in
        (y + 1, pos_list' @ pos_list))
  in
  let (_, pos_list) = str_list_to_pos_list str_list in
  List.fold ~init:(create ()) ~f:(fun acc (p, disk) ->
      put acc p (Cell.Disk disk))
    pos_list

type direction = int * int

(* val directions : direction list *)
let directions =
  [(-1, -1); ( 0, -1); ( 1, -1);
   (-1,  0);           ( 1,  0);
   (-1,  1); ( 0,  1); ( 1,  1)]

(* val reversible : t -> Pos.t -> Disk -> direction -> bool *)
(* ある場所にディスクが置けるか判定する。
   ある場所から、指定した方向に、指定した色とは異なる色のディスクが1つ以上連続し、
   最後は指定した色になれば、ディスクを置けると判定する。 *)
let reversible t pos disk (dx, dy) =
  let forward_pos pos' = Pos.create (Pos.x pos' + dx) (Pos.y pos' + dy) in
  let rec forward pos' disk' =
    let next_pos = forward_pos pos' in
    let cell = get t pos' in
    match cell with
      Cell.Disk d ->
      if d = disk' then forward next_pos disk'
      else (pos', cell)
    | _ -> (pos', cell)
  in
  let start_pos = forward_pos pos in
  let (end_pos, cell) = forward start_pos (Disk.reverse disk) in
  if start_pos = end_pos then false
  else
    match cell with
      Cell.Disk _ -> true
    | _ -> false

let can_put t pos disk =
  List.fold ~init:false ~f:(fun acc dir ->
      acc || reversible t pos disk dir) directions

(* val reverse : t -> pos -> Disk.t -> direction -> t *)

let rec reverse t pos disk dir =
  let (dx, dy) = dir in
  let next_pos = Pos.create (Pos.x pos + dx) (Pos.y pos + dy) in
  let cell = get t next_pos in
  match cell with
    Cell.Disk d ->
    if d = disk then t
    else
      let t' = put t next_pos (Cell.Disk disk) in
      (* printf "pos=%d,%d\n" (Pos.x next_pos) (Pos.y next_pos); *)
      reverse t' next_pos disk dir
  | _ -> raise @@ Runtime_error "BUG? can't reverse"

let put_and_reverse t pos disk =
  let t' = put t pos (Cell.Disk disk) in
  List.fold ~init:t' ~f:(fun t dir ->
      if reversible t pos disk dir then
        reverse t pos disk dir
      else t)
    directions

let fold ~init ~f =
  let xs = List.range 0 width in
  let ys = List.range 0 height in
  List.fold ys ~init ~f:(fun y_acc y ->
      List.fold xs ~init:y_acc ~f:(fun x_acc x ->
          f x_acc (Pos.create x y)))

let pos_list t disk =
  fold ~init:[] ~f:(fun acc pos ->
      let cell = get t pos in
      match cell with
        Cell.None ->
        if can_put t pos disk then pos :: acc
        else acc
      | _ -> acc
    )
      
let count_disk t disk =
  List.fold t ~init:0 ~f:(fun n (_, cell) ->
      match cell with
        Cell.Disk disk' -> if disk' = disk then n + 1 else n
      | _ -> n)

let board_test1 = 
  ["X X X X X X X X";
   "O O O O O X X X";
   "X O X O O X X X";
   "X O O X O O X X";
   "X O X X X X X X";
   "X X O X O X X X";
   "X X X X X X X  ";
   "X     X   X X X";]

let board_test2 = 
  ["O O O O O O O O";
   "X X X X X X X X";
   "X X O O O X X O";
   "X X X X O O X O";
   "X O O X O O O  ";
   "X O X O O O O O";
   "X O O X X O X  ";
   "X O X X X X X X";]

let board_test3 = 
  ["O X X X X X X X";
   "X X X X X X X X";
   "X X O O X O O X";
   "X X X O O O O X";
   "X O X X O O O X";
   "X O O O O O O X";
   "X O O X O O O  ";
   "X O O O O O O  ";]

let init_for_test () =
  let board = from_string_list board_test3 in
  board

let init () =
  List.fold ~init:(create ()) ~f:(fun acc (p, disk) ->
      put acc p (Cell.Disk disk))
    [((pos 3 3), Disk.White); ((pos 3 4), Disk.Black);
     ((pos 4 3), Disk.Black); ((pos 4 4), Disk.White);]

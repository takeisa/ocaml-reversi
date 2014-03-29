open Core.Std

let log message =
  Out_channel.with_file "computer.log" ~append:true
    ~f:(fun outc ->
        Out_channel.output_string outc message;
      )

(* val pos_score : Board.t -> pos -> Disk.t -> int *)
let calc_pos_score board pos disk =
  match Board.get board pos with
    Cell.None -> 0
  | Cell.Boarder -> assert false
  | Cell.Disk disk' -> 
    let sign = if disk = disk' then 1 else -1 in
    let score =
      match Pos.to_tuple pos with
        (0, 0) | (7, 0) | (0, 7) | (7, 7) -> 100
      | (0, _) | (7, _) | (_, 0) | (_, 7) -> 10
      | _ -> 1
    in
    score * sign

(* val board_score : Board.t -> Disk.t *)
let calc_board_score board disk =
  Board.fold ~init:0 ~f:(fun acc pos ->
      acc + calc_pos_score board pos disk)

(* val print_pos_list : Pos.t list -> unit *)
let print_pos_list pos_list = 
  List.iter pos_list ~f:(fun pos -> printf "%s" (Pos.to_string pos));
  print_endline ""

type move = MyMove | YourMove

(* type pos_score_opt = (Pos.t * int) option *)

(* val minmax : cmp:(int -> int -> int) -> pos_score_opt -> pos_score_opt -> pos_score_opt *)
let minmax ~cmp ppo1 ppo2 =
  match ppo1 with
    None -> ppo2
  | Some (pos1, score1) ->
    match ppo2 with
      None -> ppo1
    | Some (pos2, score2) ->
      if cmp score1 score2 then ppo2
      else ppo1

(* val pos_score_to_string : pos_score_opt -> string *)
let pos_score_to_string ppo =
  match ppo with
    None -> "None"
  | Some (pos, score) -> Printf.sprintf "(%d,%d)->%d"
                           (Pos.x pos) (Pos.y pos) score

(* val eval' : Board.t -> Disk.t -> int -> int -> (Pos.t * int) option *)
let rec eval_aux board disk max_level cur_level =
  let move = if cur_level mod 2 = 0 then MyMove else YourMove in
  let move_disk = if move = MyMove then disk else Disk.reverse disk in
  let score_cmp = if move = MyMove then (<) else (>) in
  let spc = String.make (cur_level * 2) ' ' in
  let pos_list = Board.pos_list board move_disk in
  log @@ sprintf "%scur_level=%d" spc cur_level;
  log @@ sprintf " disk=%s\n" (if move = MyMove then "MyMove" else "YourMove");
  log @@ sprintf "%spos_list length=%d\n" spc (List.length pos_list);
  (* print_pos_list pos_list; *)
  let pos_score = List.fold pos_list ~init:None ~f:(fun acc pos ->
      let board' = (Board.put_and_reverse board pos move_disk) in
      let pos_score' =
        if cur_level = max_level then
          Some (pos, calc_board_score board' disk)
        else
          match eval_aux board' disk max_level (cur_level + 1) with
            None -> Some (pos, calc_board_score board' disk)
          | Some (_, score) -> Some (pos, score)
      in
      log @@ sprintf "%s" spc;
      log @@ sprintf "%s\n" (pos_score_to_string pos_score');
      minmax ~cmp:score_cmp acc pos_score'
    ) in
  pos_score

(* val eval : Board.t -> Disk.t -> Pos.t option *)
let eval board disk =
  let pos_score = eval_aux board disk 3 0 in
  match pos_score with
    None -> None
  | Some (pos, score) -> Some pos

(*
  a b c d e f g h
0 O O O O O O O O 
1 X X X X X X X X 
2 X X O O O X X O 
3 X X X X O O X O 
4 X O O X O O O   
5 X O X O O O O O 
6 X O O X X O X   
7 X O X X X X X X 
*)


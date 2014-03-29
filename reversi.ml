open Core.Std

exception Runtime_error of string
exception Not_implemented

(* val cell_char : cell -> string *)
let cell_char cell =
  match cell with
    Cell.Disk disk -> begin
      match disk with
        Disk.Black -> "X"
      | Disk.White -> "O"
    end
  | Cell.None -> " "
  | _ -> raise (Runtime_error "not in board")

(* val print_title : unit -> unit *)
let print_title () =
  (print_endline "*** Reversi ***";
   print_endline "";)

(* val print_board : Board.t -> unit *)
let print_board board =
  (print_endline "  a b c d e f g h";
   for y = 0 to Board.height - 1 do
     printf "%d " y;
     for x = 0 to Board.width - 1 do
       let cell = Board.get board (Pos.create x y) in
         printf "%s " (cell_char cell)
     done;
     print_endline ""
   done;
   print_endline "";
  )

(* val count_disk : Board.t -> int * int *)
let count_disk board =
  let black_num = Board.count_disk board Disk.Black in
  let white_num = Board.count_disk board Disk.White in
  (black_num, white_num)

(* val print_disk_info : Board.t -> disk option -> unit *)
let print_disk_info board player =
  let (black, white) =
    match player with
      Some disk' -> 
      (match disk' with
         Disk.Black -> ("You", "Computer")
       | _ -> ("Computer", "You"))
    |None -> ("Black", "White") in
  let (black_num, white_num) = count_disk board in
  printf "X:%s [%d]  O:%s [%d]\n\n" black black_num white white_num

(* val select_black_or_white : unit -> Disk.t *)
let select_black_or_white () =
  match Random.int 2 with
        0 -> Disk.Black
      | _ -> Disk.White

(* val choose_disk : unit -> Disk.t *)
let rec choose_disk () =
  (print_string "Choose disk color (X:Black[x] / O:White[o] / Random[r]): ";
   let s = read_line () in
   match (String.lowercase s) with
     "x" -> Disk.Black
   | "o" -> Disk.White
   | "r" -> select_black_or_white ()
   | _ -> choose_disk ()
  )

(* val disk_desc : disk -> string *)
let disk_desc disk =
  match disk with
    Disk.Black -> "X:Black"
  | _ -> "O:White"

(* val print_disk_desc : disk -> string *)
let print_disk_desc disk =
  printf "\nYour disk is %s.\n\n" (disk_desc disk)

type status = {
  board         : Board.t;
  board_history : Board.t list;
  player        : Disk.t;
  turn          : Disk.t;
  turn_no       : int;
  quit          : bool;
}

(* val print_turn_info : status -> unit *)
let print_turn_info {player; turn; turn_no; _} =
  printf "Turn %d %s\n\n" turn_no
    (if player = turn then "You" else "Computer")

(* val next_turn : Disk.t -> Disk.t *)
let next_turn disk =
  if disk = Disk.Black then
    Disk.White
  else
    Disk.Black

type command = Cmd_undo
             | Cmd_put of Pos.t
             | Cmd_pass
             | Cmd_quit

(* val parse_pos : string -> Pos.t option *)
let parse_pos s =
  let len = String.length s in
  if len < 2 || len > 2 then None
  else
    let (xc, yc) = (String.get s 0, String.get s 1) in
    if xc < 'a' || xc > 'h'
       || yc < '0' || yc > '7' then None
    else
      Some (Pos.create (Char.ascending xc 'a') (Char.ascending yc '0'))

(* val reversi_pos : Pos.t -> string *)
let reversi_pos_string pos =
  let x_to_char x =
    Char.of_int_exn (Char.to_int 'a' + x) in
  let y_to_char y =
    Char.of_int_exn (Char.to_int '0' + y) in
  Printf.sprintf "%c%c" (x_to_char (Pos.x pos)) (y_to_char (Pos.y pos))

(* val can_put_disk : Board.t -> Disk.t -> bool *)
let can_put_disk board disk =
  let pos_list = Board.pos_list board disk in
  not (List.is_empty pos_list)

(* val can_put : board -> bool *)
let can_put board =
  List.fold ~init:false ~f:(fun acc disk ->
      acc || can_put_disk board disk)
    [Disk.Black; Disk.White]

(* val confirm : string -> unit *)
let confirm message =
  print_endline message;
  ignore (read_line ())

(* val get_player_command : status -> command *)
let rec get_player_command status =
  match status with
    {board; board_history; player; turn; turn_no; quit} ->
    if not (can_put_disk board player) then
      (confirm "You can't put a disk. Pass! [Enter]";
       Cmd_pass
      )
    else
      let can_undo = not (List.is_empty board_history) in
      (printf "Input disk position [a1-h8] /%s Quit[q] :"
         (if can_undo then " Undo[u] /" else "");
       let s = read_line () in
       print_endline "";
       match (String.lowercase s) with
         "q" -> Cmd_quit
       | "u" when can_undo -> Cmd_undo
       | s ->
         let pos = parse_pos s in
         match pos with
           None -> get_player_command status
         | Some pos' ->
           if not (Board.can_put board pos' turn) then
             (printf "[Error] Invalid position : %s\n\n" s;
              get_player_command status)
           else
             Cmd_put pos'
      )

(* val get_player_command : status -> command *)
let get_computer_command {board; board_history; player; turn; turn_no} =
  let pos = Computer.eval board turn in
  match pos with
    Some pos' ->
    confirm (Printf.sprintf "I put a disk at %s. [Enter]" (reversi_pos_string pos'));
    Cmd_put pos'
  | None ->
    confirm "I can't put a disk. pass! [Enter]";
    Cmd_pass
 
(* val get_command : status -> command *)
let get_command status =
  match status with
    {player; turn; _} ->
    if player = turn then
      get_player_command status
    else
      get_computer_command status

(* val undo : status -> status *)
let undo {board; board_history; player; turn; turn_no; quit} =
  match board_history with
    board0 :: board1 :: board_history' ->
    {board = board1; board_history = board_history';
     player; turn; turn_no = turn_no - 2; quit}
  | _ -> raise @@ Runtime_error "BUG? board_history is empty"

(* val play : status -> status *)
let rec play status =
  match status with
    {board; board_history; player; turn; turn_no; quit} ->
    print_board board;
    print_disk_info board (Some player);
    print_turn_info status;
    if not (can_put board) then
      status
    else
      let command = get_command status in
      match command with
        Cmd_put pos ->
        let next_board = Board.put_and_reverse board pos turn in
        play {board = next_board;
              board_history = board :: board_history;
              player; turn = (next_turn turn); turn_no = turn_no + 1;
              quit}
      | Cmd_pass ->
        play {status with turn = (next_turn turn); turn_no = turn_no + 1;
                          board_history = board :: board_history;}
      | Cmd_undo -> play @@ undo status
      | Cmd_quit -> {status with quit = true}

let () =
  Random.self_init ();
  print_title ();
  let board = Board.init () in
  print_board board;
  print_disk_info board None;
  let player = choose_disk () in
  print_disk_desc player;
  let status = {board = board; board_history = [];
                player = player; turn = Disk.Black; turn_no = 1;
                quit = false} in
  let {board;player;quit;_} = play status in
  if quit then
    print_endline "Quit game."
  else
    let (black_num, white_num) = count_disk board in
    let you_win =
      (if player = Disk.Black then (>) else (<)) black_num white_num in
    print_endline
      (if black_num = white_num then
         "End in a draw."
       else
       if you_win then "You win."
       else "You lose.")
    

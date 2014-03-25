exception Curses_error

module A = Curses.A
module Color = Curses.Color

(* fun_exn : (unit -> bool) -> unit *)
let fun_exn f =
  if f () then ()
  else raise Curses_error

(* Initialize/Finalize *)
let initscr = Curses.initscr
let endwin = Curses.endwin

(* Output *)
let refresh () =
   fun_exn (fun () -> Curses.refresh ())
let mvwaddstr w y x s =
  fun_exn (fun () -> Curses.mvwaddstr w y x s)
let addstr s =
  fun_exn (fun () -> Curses.addstr s)

(* Clear *)
let clear = Curses.clear

(* Move *)
let move y x =
  fun_exn (fun () -> Curses.move y x)

(* Attribute *)
let attr_on = Curses.attr_on
let attr_off = Curses.attr_off

(* Keyboard mode *)
let echo () =
  fun_exn (fun () -> Curses.echo ())
let noecho () =
  fun_exn (fun () -> Curses.noecho ())
let cbreak () =
  fun_exn (fun () -> Curses.cbreak ())
let nocbreak () =
  fun_exn (fun () -> Curses.cbreak ())
let raw () =
  fun_exn (fun () -> Curses.raw ())
let noraw () =
  fun_exn (fun () -> Curses.noraw ())

(* Keyboard input *)
let getch = Curses.getch
let getnstr = Curses.getnstr

(* Color *)
let has_colors = Curses.has_colors
let start_color () =
  fun_exn (fun () -> Curses.start_color ())
let color_pairs () = Curses.color_pairs ()
let colors = Curses.colors
let init_pair n f b =
  fun_exn (fun () -> Curses.init_pair n f b)
let init_color n r g b =
  fun_exn (fun () -> Curses.init_color n r g b)


(* [quit_game] prints an error message and exits the game *)
let quit_game adv = 
  let _ = 
    (print_endline "You have successfully exited the game. Goodbye!") in exit 0

(* [continue_game adv id st] continues the the current adventure [adv] 
   starting from room [id] and state [st] *)
let rec continue_game adv id st = 
  Adventure.description adv id |> print_endline;
  match (read_line () |> Command.parse) with 
  | exception Command.Malformed -> 
    let _ = 
      print_endline "Command could not be understood. Please try again" in 
    continue_game adv id st
  | Command.Quit -> quit_game adv
  | Command.Go field -> let go_phrase = String.concat " " field in 
    match st |> State.go go_phrase adv with 
    | Illegal -> let _ = print_endline "Illegal" in continue_game adv id st
    | Legal field -> continue_game adv (State.current_room_id field) field

(** [play_game f] starts the adventure in file [f]. *)
let play_game f =
  let adventure = Yojson.Basic.from_file f |> Adventure.from_json in 
  let start = Adventure.start_room adventure in 
  let initialstate = State.init_state adventure in
  continue_game adventure start initialstate


(* have result *)
(* if result is illegal, say could not understand and try *)
(* if legal, take new state, print the description, and rerun func using new state as state *)


(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to the 3110 Text Adventure Game engine.\n");
  print_endline "Please enter the name of the game file you want to load.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game file_name

(* Start the game *)
let () = main ()
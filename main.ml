let rec play_game state = 
   print_endline ("It is "^State.turn state^"'s turn.");
   print_endline "You can either check the score with 'score' or 
   move a piece with 'move'.";
   print_endline "What would you like to do?";
   match read_line () with
   | "score" -> failwith ""
   | "move" -> failwith ""
   | _ -> print_endline "That's not a valid command"; play_game state

(* [main] starts the chess game. It asks for the players' names, asks 
   which player is white and which is black, initilizes the board, and allows
   the players to play *)
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to our Chess simulator.\n");
  print_endline "Please enter player one's name.\n";
  print_string  "> ";
  let player_one = read_line () in 
   print_endline "Please enter player two's name.\n";
   print_string  "> ";
  let player_two = read_line () in 
  let initial_state = State.init_state player_one player_two in 
  play_game initial_state



(* Execute the game engine. *)
let () = main ()

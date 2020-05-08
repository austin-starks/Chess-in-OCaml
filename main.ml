(* TODO:
   Thurs: 
      - handle "NotAPiece" and "IllegalMove" errors 
   Fri
      - prevent player one from moving player two pieces 
      - implement the difference pieces function
      - check when player is in check or checkmate
   Sat
      - implement ability for pawn to change to another piece
      - a player should be able to castle
   *)

let rec play_game state = 
   print_endline ("It is "^State.turn_player_name state^"'s turn.");
   Chessboard.print_board (State.current_board state);
   print_endline "What would you like to do?";
   print_string "> ";
   match String.lowercase_ascii (read_line ()) with
   | "score" -> print_endline (State.score state); play_game state
   | x -> let new_state = State.move_piece state x in play_game new_state

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
  print_endline "\nInstructions:";
  print_endline "There are two valid commands. The first command is score";
  print_endline "and you can use it by typing 'score'. Otherwise you can move";
  print_endline "your pieces by typing in two positions in the form of 'x# x#'";
  print_endline "where 'x' is a letter from [A-H] and # is a number from [1-8]"; 
  print_endline "For example, 'a2 a4' is a command that moves the piece on a2";
  print_endline "to a4 (if it is a valid move). If it's not a valid move, the";
  print_endline "engine will ask for another move for that player.\n";
  let initial_state = State.init_state player_one player_two in 
  play_game initial_state



(* Execute the game engine. *)
let () = main ()

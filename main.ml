(* TODO:
   - end the game when the king is taken 
   - fix queen moving through objects bug
*)
let ignore s = ()

let rec play_game state = 
  match State.score state |> ignore with 
  | exception State.P1Checkmate -> print_endline "Player 1 has checkmate";
  | exception State.P2Checkmate -> print_endline "Player 2";
  |_ -> ();
    print_endline ("It is "^State.turn_player_name state^"'s turn.");
    Chessboard.print_board (State.current_board state);
    print_endline "What would you like to do?";
    print_string "> ";
    match String.lowercase_ascii (read_line ()) with
    | "score" -> print_endline (State.score state); play_game state
    | "stalemate" -> print_endline "The game ends in a draw \n"; 
      print_endline (State.score state); exit 0
    | x -> match State.move_piece state x with 
      | new_state -> play_game new_state
      | exception Chessboard.NotAPiece -> 
        error_handling "There is no piece at that positiion." state; 
      | exception Chessboard.IllegalMoveError -> 
        error_handling "That piece can not move in that way." state; 
      | exception State.InvalidCommand -> 
        error_handling "That piece can not move in that way." state; 
        (* | exception State.Checkmate -> print_endline "A player has won"; exit 0 *)

and error_handling msg state = 
  print_endline ("\n"^msg); 
  print_endline "Please try a different move."; 
  play_game state




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
  print_endline "There are three valid commands. The first command is score";
  print_endline "and you can use it by typing 'score'. The second command is";
  print_endline "stalement. This is used when neither play can no longer win";
  print_endline "because there are no more valid moves. Typing in this command";
  print_endline "will result in a draw. Otherwise you can move ";
  print_endline "pieces by typing in two positions in the form of 'x# x#'";
  print_endline "where 'x' is a letter from [A-H] and # is a number from [1-8]"; 
  print_endline "For example, 'a2 a4' is a command that moves the piece on a2";
  print_endline "to a4 (if it is a valid move). If it's not a valid move, the";
  print_endline "engine will ask for another move for that player.\n \n";
  print_endline (player_one ^ " is white/yellow, and is on bottom (corresponding to rows 1-2)");
  print_endline (player_two ^ " is black/blue, and is on top (corresponding to rows 7-8) \n");
  let initial_state = State.init_state player_one player_two in 
  play_game initial_state



(* Execute the game engine. *)
let () = main ()

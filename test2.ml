open State

let state = (init_state "Austin" "Justin" )

(* Testing the initial chessboard is a regular chessboard *)
let () = assert (current_board state = [
  [|Rook Black; Knight Black; Bishop Black; Queen Black; 
    King Black; Bishop Black; Knight Black; Rook Black|];
  [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
    Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|Pawn White; Pawn White; Pawn White; Pawn White; 
    Pawn White; Pawn White; Pawn White; Pawn White|];
  [|Rook White; Knight White; Bishop White; Queen White; 
    King White; Bishop White; Knight White; Rook White|];
] )

(* Testing that move piece function from state moves the piece. *)
let state = State.move_piece state "a2 a4"

let () = assert (current_board state = [
  [|Rook Black; Knight Black; Bishop Black; Queen Black; 
    King Black; Bishop Black; Knight Black; Rook Black|];
  [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
    Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|Pawn White; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; Pawn White; Pawn White; Pawn White; 
    Pawn White; Pawn White; Pawn White; Pawn White|];
  [|Rook White; Knight White; Bishop White; Queen White; 
    King White; Bishop White; Knight White; Rook White|];
] )

(* Testing that the player's turn changes after making a valid move *)
let player_name = turn_player_name state
let state = State.move_piece state "b7 b5"
let player_name' = turn_player_name state
let () = assert (player_name <> player_name')

(* Testing that the score returns a string representation of the score for
both players *)
let points = score state 

let () = assert (points = "Austin has 0 points.\nJustin has 0 points.\n")
let state = State.move_piece state "a4 b5"
let points = score state 
let () = assert (points = "Austin has 1 points.\nJustin has 0 points.\n")
let state = State.move_piece state "c7 c6"
let state = State.move_piece state "a1 a7"
let state = State.move_piece state "c6 b5"
let points = score state 
let () = assert (points = "Austin has 2 points.\nJustin has 1 points.\n")
let state = State.move_piece state "a7 a8"
let points = score state 
let () = assert (points = "Austin has 7 points.\nJustin has 1 points.\n")

(* Testing invalid commands throw an exception *)
let _ = match State.move_piece state "a7 a9" with 
  | exception InvalidCommand -> assert true
  | _ -> assert false

let _ = match State.move_piece state "bob builder" with 
  | exception InvalidCommand -> assert true
  | _ -> assert false


(* We have two testing approaches. For one approach, we decided to test the 
   functionality of every individual piece and the board itself. Specifically, 
   they are tested to ensure they can move in with the appropiate behavior. In 
   particular, we are testing:

   Pawn:
   -	Can move two spaces on first move
   -	Can move one space normally
   -	Can move diagonally when taking pieces

   Rook
   -	Can move horizontally
   -	Can move vertically

   Queen
   -	Can move horizontally
   -	Can move vertically
   -	Can move diagonally

   Knight
   -	Can move like a normal knight

   Bishop
   -	Can move diagonally

   King
   -	Can move horizontally
   -	Can move vertically
   -	Can move diagonally

   General
   - Can take pieces
   - Can castle

   Also, we are testing to ensure that pieces cannot move inappropiately; they 
   can only move in spaces that are legal in the game of chess. For example, a 
   rook cannot move horizontally, and a knight can't move vertically. In 
   addition,

   Lastly, we are also testing the functiality of State to make sure it works 
   as intended. 
*)


open Chessboard
open State

let board = Chessboard.initialize_chessboard ()

(* Testing move pawn two spaces*)
let _  = (Chessboard.move_piece board "E2" "E4") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(* Testing move black pawn 1 space*)
let _  = (Chessboard.move_piece board "D7" "D6") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; Pawn Black; None; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(* Testing pawn can move diagonally when taking pieces *)
let _  = (Chessboard.move_piece board "E4" "E5") 
let _  = (Chessboard.move_piece board "D6" "E5") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(* Testing can move rook vertically *)
let _  = (Chessboard.move_piece board "A2" "A4") 
let _  = (Chessboard.move_piece board "A1" "A3") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|Rook White; None;None; None; None; None; None; None;|];
    [|None; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(* Testing can move rook horizontally *)
let _  = (Chessboard.move_piece board "A3" "C3") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; None; None; None; None; None;|];
    [|None; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(*   Testing can move queen vertically *)
let _  = (Chessboard.move_piece board "D8" "D6") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; Queen Black; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; None; None; None; None; None;|];
    [|None; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(*   Testing can move queen horizontally *)
let _  = (Chessboard.move_piece board "D6" "A6") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|Queen Black; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; None; None; None; None; None;|];
    [|None; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

(* Testing can move queen diagonally *)
let _  = (Chessboard.move_piece board "A6" "F1") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; None; None; None; None; None;|];
    [|None; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; Bishop White; Queen White; 
      King White; Queen Black; Knight White; Rook White|];
  ])


(* Testing can move knight*)
let _  = (Chessboard.move_piece board "g1" "f3") 
let _  = (Chessboard.move_piece board "d2" "d3") 
let _  = (Chessboard.move_piece board "f3" "d2") 
let _  = (Chessboard.move_piece board "d2" "f1") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; Pawn White; None; None; None; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; Bishop White; Queen White; 
      King White; Knight White; None; Rook White|];
  ])

(* Testing can move bishops diagnally up right *)
let _  = (Chessboard.move_piece board "c1" "e3") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; Pawn White; Bishop White; None; None; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; None; Queen White; 
      King White; Knight White; None; Rook White|];
  ])

(* Testing can move bishops diagnally up left *)
let _  = (Chessboard.move_piece board "e3" "c5") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; Bishop White; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|None; None; Rook White; Pawn White; None; None; None; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; None; Queen White; 
      King White; Knight White; None; Rook White|];
  ])

(* Testing can move bishops diagnally down left *)
let _  = (Chessboard.move_piece board "c5" "a3") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|Bishop White; None; Rook White; Pawn White; None; None; None; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; None; Queen White; 
      King White; Knight White; None; Rook White|];
  ])

(* Testing castle *)
let _  = (Chessboard.move_piece board "f1" "g3") 
let _  = (Chessboard.move_piece board "e1" "g1") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|Bishop White; None; Rook White; Pawn White; None; None; Knight White; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; None; Queen White; 
      None; Rook White; King White; None|];
  ])
(* Testing king horizontally *)
let _  = (Chessboard.move_piece board "g1" "h1") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; None;|];
    [|Bishop White; None; Rook White; Pawn White; None; None; Knight White; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; Pawn White|];
    [|None; Knight White; None; Queen White; 
      None; Rook White; None; King White|];
  ])

(* Testing king vertically *)
let _  = (Chessboard.move_piece board "h2" "h4") 
let _  = (Chessboard.move_piece board "h1" "h2") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; Pawn White;|];
    [|Bishop White; None; Rook White; Pawn White; None; None; Knight White; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; King White|];
    [|None; Knight White; None; Queen White; 
      None; Rook White; None; None|];
  ])

(* Testing king diagnally *)
let _  = (Chessboard.move_piece board "h2" "g1") 
let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; None; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; None; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|Pawn White; None; None; None; None; None; None; Pawn White;|];
    [|Bishop White; None; Rook White; Pawn White; None; None; Knight White; None;|];
    [|None; Pawn White; Pawn White; None; 
      None; Pawn White; Pawn White; None|];
    [|None; Knight White; None; Queen White; 
      None; Rook White; King White; None|];
  ])



(* Now, moving on to testing State *)

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


let c2 = initialize_chessboard ()

(* Test that a piece cannot move to the same position as another piece of the 
   same color - rook *)
let _ = match Chessboard.move_piece c2 "a1" "a2" with 
  | exception SameColorMoveError -> assert true 
  | _ -> assert false

(* Test that a piece cannot move to the same position as another piece of the 
   same color - bishop *)
let _ = match Chessboard.move_piece c2 "c1" "b2" with 
  | exception SameColorMoveError -> assert true 
  | _ -> assert false

(* Test that a piece cannot move to the same position as another piece of the 
   same color - king *)
let _ = match Chessboard.move_piece c2 "e1" "d1" with 
  | exception SameColorMoveError -> assert true 
  | _ -> assert false

(* Test that a piece cannot move to the same position as another piece of the 
   same color - queen *)
let _ = match Chessboard.move_piece c2 "d1" "d2" with 
  | exception SameColorMoveError -> assert true 
  | _ -> assert false

(* Test that a piece cannot move to the same position as another piece of the 
   same color - queen *)
let _ = match Chessboard.move_piece c2 "g1" "e2" with 
  | exception SameColorMoveError -> assert true 
  | _ -> assert false



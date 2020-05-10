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
   rook cannot move horizontally, and a knight can't move vertically. 

   Lastly, we are also testing the functionality of State to make sure it works 
   as intended. Main.ml was tested by hand and by playing the game of chess.

   We could not figure out how to make a traditional OUnit test suite with 
   this project. Because we are using an array for the chessboard and are 
   mutating the array, it didn't seem possible to write tests like we did 
   throughout the semester. Thus, instead we chose to do an A1-style approach to
   testing by writing assert statements and passing those tests. 

   These tests prove our system is correct. Even though most of these tests are 
   glass-box tests (as we know how the functions are implemented and how a 
   chessboard is represented), the tests are still robust. We test the 
   functionality of the chessboard piece-by-piece. This includes edge cases such
   as the ability of pawns to move two spaces as their first move, but only one
   space in every subsequent move and the ability for pawns to move diagonally 
   (but only if it can take a piece). Moreover, we test the state to make sure 
   it works as intended, and have play-tested a full game with this system.
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

(* Testing can move bishops diagonally up right *)
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

(* Testing can move bishops diagonally up left *)
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

(* Testing can move bishops diagonally down left *)
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

(* Testing king diagonally *)
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


let c4 = initialize_chessboard ()

(* Test that moving a None piece results in an error *)
let _ = match Chessboard.move_piece c4 "c4" "c5" with 
  | exception NotAPiece -> assert true 
  | _ -> assert false

(* Test that pieces cannot be moved to random spaces*)
let _ = match Chessboard.move_piece c4 "a1" "h5" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

let _ = match Chessboard.move_piece c4 "g1" "b3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that rooks cannot move through pieces *)
let _ = match Chessboard.move_piece c4 "a1" "a3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that bishops cannot move through pieces *)
let _ = match Chessboard.move_piece c4 "c1" "a3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that queens cannot move through pieces *)
let _ = match Chessboard.move_piece c4 "d1" "d4" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that knights can move through pieces *)
let _ = match Chessboard.move_piece c4 "g1" "f3" with 
  | exception IllegalMoveError -> assert false 
  | () -> assert true

(* Now testing different illegal moves *)
let c3 = Chessboard.initialize_chessboard ()

let _  = (Chessboard.move_piece c3 "e2" "e4") 

(* Pawns *)
(* Test that pawn can only move two spaces during it's first move *)
let _ = match Chessboard.move_piece c3 "e4" "e6" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that pawn can not move diagonal if it is not taking a piece *)
let _ = match Chessboard.move_piece c3 "e4" "f5" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that pawn can not move backwards *)
let _ = match Chessboard.move_piece c3 "e4" "e3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Rooks *)
let _  = (Chessboard.move_piece c3 "a2" "a4") 
let _  = (Chessboard.move_piece c3 "a1" "a3") 
let _  = (Chessboard.move_piece c3 "a3" "c3") 

(* Test that rooks can not move diagnal *)
let _ = match Chessboard.move_piece c3 "c3" "d4" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that rooks can not move like knights *)
let _ = match Chessboard.move_piece c3 "c3" "e2" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Queens *)
let _  = (Chessboard.move_piece c3 "f7" "f5") 
let _  = (Chessboard.move_piece c3 "f5" "e4") 
let _  = (Chessboard.move_piece c3 "d1" "e2") 
let _  = (Chessboard.move_piece c3 "e2" "e4") 

(* Test that queens can not move like knights *)
let _ = match Chessboard.move_piece c3 "e4" "g5" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that queens can not move randomly *)
let _ = match Chessboard.move_piece c3 "e4" "f7" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Knight *)
let _  = (Chessboard.move_piece c3 "g1" "f3") 

(* Test that knights can not move vertically *)
let _ = match Chessboard.move_piece c3 "f3" "f4" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that knights can not move horizontally *)
let _ = match Chessboard.move_piece c3 "f3" "e3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* King *)
(* Test that Kings can not move more than one space *)
let _ = match Chessboard.move_piece c3 "e1" "e3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Bishop *)
(* Test that Bishops can not move horizontally *)
let _ = match Chessboard.move_piece c3 "c1" "d1" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

let c4 = initialize_chessboard ()

(* Test that moving a None piece results in an error *)
let _ = match Chessboard.move_piece c4 "c4" "c5" with 
  | exception NotAPiece -> assert true 
  | _ -> assert false

(* Test that pieces cannot be moved to random spaces*)
let _ = match Chessboard.move_piece c4 "a1" "h5" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

let _ = match Chessboard.move_piece c4 "g1" "b3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that rooks cannot move through pieces *)
let _ = match Chessboard.move_piece c4 "a1" "a3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that bishops cannot move through pieces *)
let _ = match Chessboard.move_piece c4 "c1" "a3" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that queens cannot move through pieces *)
let _ = match Chessboard.move_piece c4 "d1" "d4" with 
  | exception IllegalMoveError -> assert true 
  | _ -> assert false

(* Test that knights can move through pieces *)
let _ = match Chessboard.move_piece c4 "g1" "f3" with 
  | exception IllegalMoveError -> assert false 
  | () -> assert true
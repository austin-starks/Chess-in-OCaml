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


   We are also testing the functiality of State to make sure it works at intended. 
   Considering State is responsible for making the game actually proceed, it is the
   biggest part towards making a complete game. Testing state will also ensure the 
   necessary exceptions are actually thrown. *)


open Chessboard
(* open State *)

let board = Chessboard.initialize_chessboard ()

(* Testing move pawn two spaces*)
let _  = (move_piece board "E2" "E4") 

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
let _  = (move_piece board "D7" "D6") 

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
let _  = (move_piece board "E4" "E5") 
let _  = (move_piece board "D6" "E5") 

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
let _  = (move_piece board "A2" "A4") 
let _  = (move_piece board "A1" "A3") 
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
let _  = (move_piece board "A3" "C3") 
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
let _  = (move_piece board "D8" "D6") 
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
let _  = (move_piece board "D6" "A6") 
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
let _  = (move_piece board "A6" "F1") 
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
let _  = (move_piece board "g1" "f3") 
let _  = (move_piece board "d2" "d3") 
let _  = (move_piece board "f3" "d2") 
let _  = (move_piece board "d2" "f1") 

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
let _  = (move_piece board "c1" "e3") 
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
let _  = (move_piece board "e3" "c5") 
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
let _  = (move_piece board "c5" "a3") 
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
let _  = (move_piece board "f1" "g3") 
let _  = (move_piece board "e1" "g1") 

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
let _  = (move_piece board "g1" "h1") 
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
let _  = (move_piece board "h2" "h4") 
let _  = (move_piece board "h1" "h2") 

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
      None; Rook White; None; King White|];
  ])

(* Testing king diagnally *)
let _  = (move_piece board "h2" "g1") 

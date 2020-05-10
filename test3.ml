open Chessboard

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
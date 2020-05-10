
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
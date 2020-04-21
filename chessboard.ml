type color = Black | White

type piece = 
  | Pawn of color 
  | Knight of color
  | Bishop of color
  | King of color 
  | Queen of color 
  | Rook of color 
  | None

type t = piece list list

type position = {
  letter : string;
  number: int
}

exception IllegalMoveError 

exception NoPiecePresentError 

exception SameColorMoveError


let initialize_chessboard = [
  [Rook Black; Knight Black; Bishop Black; Queen Black; 
    King Black; Bishop Black; Knight Black; Rook Black];
  [Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
    Pawn Black; Pawn Black; Pawn Black; Pawn Black];
  [None; None; None; None; None; None; None; None;];
  [None; None; None; None; None; None; None; None;];
  [None; None; None; None; None; None; None; None;];
  [None; None; None; None; None; None; None; None;];
  [Pawn White; Pawn White; Pawn White; Pawn White; 
    Pawn White; Pawn White; Pawn White; Pawn White];
  [Rook White; Knight White; Bishop White; Queen White; 
    King White; Bishop White; Knight White; Rook White];
]

let is_valid_move piece pos1 pos2 = 
  match piece with 
  | Pawn Black-> failwith ""
  | _ -> failwith ""

(** [parse_position pos] is a record representing the string pos
  Requires: pos to be a1, a2, ... a8, b1, ..., h7, or h8  *)
let parse_position pos = failwith ""

let move_piece t pos1 pos2 = failwith "Unimplemented"

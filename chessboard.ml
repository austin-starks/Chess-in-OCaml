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
  | Pawn Black-> if pos1.letter = pos2.letter && (
      (pos1.number = pos2.number -1)
      || (pos1.number = 7 && pos2.number = 5)) then true else false
  | Pawn White -> if pos1.letter = pos2.letter && (
      (pos1.number = pos2.number +1)
      || (pos1.number = 2 && pos2.number = 4)) then true else false
  | Rook _ -> if pos1.letter = pos2.letter || pos1.number = pos2.number 
    then true else false
  | _ -> failwith ""

(** [parse_position pos] is a record representing the string pos
    Requires: pos to be a1, a2, ... a8, b1, ..., h7, or h8  *)
let parse_position pos = {
  letter = String.sub pos 0 1;
  number = String.sub pos 1 2 |> int_of_string
} 

(** [pos_letter_assoc_list] is an association list that maps positions to
    numbers *)
let pos_letter_assoc_list = 
  [("A", 1);  ("B", 2);  ("C", 3);  ("D", 4);  
   ("E", 5);  ("F", 6);  ("G", 7);  ("H", 8); ]

let move_piece t pos1 pos2 = failwith "Unimplemented"

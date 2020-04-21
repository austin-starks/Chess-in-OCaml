type color = Black | White

type piece = 
  | Pawn of color 
  | Knight of color
  | Bishop of color
  | King of color 
  | Queen of color 
  | Rook of color 
  | None

type t = piece list

type position = string

exception IllegalMoveError 

exception NoPiecePresentError 

exception SameColorMoveError


let initialize_chessboard = []

let move_piece t pos1 pos2 = failwith "Unimplemented"
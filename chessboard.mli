(** The abstract type representing a chessboard *)
type t 

(** The type representing a chess piece on the board *)
type piece = Pawn | Knight | Bishop | King | Queen | Rook 

(** Raised when the player makes a move that is not allowed in chess *)
exception InvalidMove 


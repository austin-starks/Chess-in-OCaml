(** The abstract type representing a chessboard *)
type t 

(** The type representing the color of the piece *)
type color = Black | White

type position

(** The type representing a chess piece on the board *)
type piece = 
  | Pawn of color 
  | Knight of color
  | Bishop of color
  | King of color 
  | Queen of color 
  | Rook of color 
  | None

(** Raised when the player makes a move that is not allowed in chess *)
exception IllegalMoveError 

(** Raised when the player attempts to move a piece that isn't there (attempts)
    to move a None piece *)
exception NoPiecePresentError 

(** Raised when a piece is trying to move to a board space containing a d piece 
    with that same color *)
exception SameColorMoveError

(** [initialize_chessboard] is a chessboard of pieces in the starting 
    arrangement *)
val initialize_chessboard : t 

(** [move_piece t old_pos new_pos] alters the chessboard with the piece at 
    [old_pos] moved to [new_pos]. 

    Raises [IllegalMoveError] if that piece cannot move from [old_pos] to [new_pos].
    Raises [NoPiecePresentError] if there is no piece in [old_pos] 
    Raises [SameColorMoveError] if a piece is moved to a position where a piece of 
    the same color is already on.  *)
val move_piece : t -> string -> string -> t

(** [is_valid_move p old_pos new_pos] checks to see if a move from old position
    [old_pos] to new position [new_pos] is a valid chess move *)
val is_valid_move : piece -> position -> position -> bool

(** 
    Representation of a chessboard. 

    This module represents a chessboard and the pieces on the board. it includes
    functionality to create a chessboard, move pieces, and print the chessboard.
*)

(** The type representing the color of the piece *)
type color = Black | White

(* Internal representation of a position *)
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

(** The abstract type representing a chessboard *)
type t = piece array list

(** Raised when the player makes a move that is not allowed in chess *)
exception IllegalMoveError 

(** Raised when a string doesn't represent a piece *)
exception NotAPiece

(** Raised when a piece is trying to move to a board space containing a d piece 
    with that same color *)
exception SameColorMoveError

(** [initialize_chessboard ()] is a chessboard of pieces in the starting 
    arrangement *)
val initialize_chessboard : unit -> t 

(** [move_piece t old_pos new_pos] alters the chessboard with the piece at 
    [old_pos] moved to [new_pos]. 

    Raises [IllegalMoveError] if that piece cannot move from [old_pos] to [new_pos].
    Raises [SameColorMoveError] if a piece is moved to a position where a piece of 
    the same color is already at the position.  *)
val move_piece : t -> string -> string -> unit

(** [get_piece_from_string] is the piece that represents a string. For example,
    typing "Rook Black" returns the piece Rook Black
    Raises NotAPiece if string doesn't represent a piece *)
val get_piece_from_string : string -> piece

(** [is_valid_move p old_pos new_pos] checks to see if a move from old position
    [old_pos] to new position [new_pos] is a valid chess move *)
val is_valid_move : t -> piece -> position -> position -> bool

(** [get_piece t pos] gets the chess piece in state [t] at position [pos] *)  
val get_piece : t -> position -> piece

(** [parse_position pos] converts the string [pos] to an internal representation
    of a position
    Requires: pos to be a1, a2, ... a8, b1, ..., h7, or h8  *)
val parse_position: string -> position

(** [count_pieces t] counts the pieces on the board for a particular color *)
val count_pieces: t -> string -> (string*int) list

(** [print_board t] prints the current chessboard to the terminal *)
val print_board: t -> unit

(** [exchange_pawns t] exchanges all pawns in the first and last row with 
    a piece of the player's choice *)
val exchange_pawns: t -> unit
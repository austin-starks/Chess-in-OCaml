(** 
   Representation of state of chess game.

   This module represents the state of a chess game that is being played from 
   game. It includes who's turn it is, what pieces are on the board (and which
   were taken), properties of the players (such as the score and their names), 
   and functions that cause the state to change. *)

(** The abstract type of values representing the game state. *)
type t

val init_state : Chessboard.t -> t

val turn : t 

val player_name: t

val score : t -> int

val current_board: t

val take_piece : t -> t

val valid_move: t -> bool

val check : t -> bool

val checkmate: t -> bool
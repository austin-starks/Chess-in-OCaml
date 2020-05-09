(** 
   Representation of the state of a chess game.

   This module represents the state of a chess game that is being played from 
   game. It includes who's turn it is, what pieces are on the board (and which
   were taken), properties of the players (such as the score and their names), 
   and functions that cause the state to change. *)

open Chessboard

(** The type representing the people playing the game *)  
type person = string

(** The abstract type of values representing the game state. *)
type t 

(** An exception thrown when the state processes an invalid command *)
exception InvalidCommand

(** An exception thrown when player 1 takes player 2's king *)  
exception P1Checkmate of person

(** An exception thrown when player 2 takes player 1's king *)  
exception P2Checkmate of person


(** [init_state p1 p2] is the initial state of the the chess game [g]. In this 
    state, the game is set up as a normal chessboard, with white being the 
    person to be able to make the first move. *)
val init_state : string -> string -> t

(* [turn_player_name t] is the turn of the player who needs to move next in state [s] *)
val turn_player_name : t -> person

(* [score p s] is the current score of both players in state [s] as calculated by
    the chess piece relative value system *)
val score : t -> string

(* [current_board s] is the current board represented by state [s] *)
val current_board: t -> Chessboard.t

(* [move_piece t pos] alters the chessboard in the state [t] by the position 
change [pos] *)
val move_piece: t -> string -> t
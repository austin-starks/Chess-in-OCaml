(** 
   Representation of state of chess game.

   This module represents the state of a chess game that is being played from 
   game. It includes who's turn it is, what pieces are on the board (and which
   were taken), properties of the players (such as the score and their names), 
   and functions that cause the state to change. *)

(** The abstract type of values representing the game state. *)
open Chessboard

type t

(** The type representing the people playing the game *)  
type person = string

(** [init_state g] is the initial state of the the chess game [g]. In this 
    state, the game is set up as a normal chessboard, with white being the 
    person to be able to make the first move. *)
val init_state : Chessboard.t -> t

(* [turn_player_name t] is the turn of the player who needs to move next in state [s] *)
val turn_player_name : t -> person

(* [player_name p] is the name of the player [p] *)
val player_name: person

(* [score p s] is the current score of person [p] in state [s] as calculated by
    the chess piece relative value system *)
val score : t -> person -> int

(* [current_board s] is the current board represented by state [s] *)
val current_board: t -> Chessboard.t

(* [check s] determines whether the current state [s] has a player in check *)
val check : t -> bool

(* [check s] determines whether the current state [s] has a player in checkmate *)
val checkmate: t -> bool
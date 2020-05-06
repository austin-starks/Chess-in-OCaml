type person = string
type piece = string 
type num_piece = int

exception InvalidCommand

type t = {
  player_turn: person;
  players: person list;
  score: (person*int) list;
  current_board: Chessboard.t;
  p1_taken_pieces: (string*int) list;
  p2_taken_pieces: (string*int) list
}

let init_state person1 person2 = 
  {
  player_turn= person1;
  players= [person1; person2];
  score= [(person1, 0); (person2, 0)];
  current_board= Chessboard.initialize_chessboard ();
  p1_taken_pieces= [];
  p2_taken_pieces= []
  }

  


let turn_player_name t =
  t.player_turn

let score t =
  match t.score with 
  | h::t::[] -> (fst h) ^ "'s score is: " ^ string_of_int (snd h) ^ " \n"^
                (fst t) ^ "'s score is: " ^ string_of_int (snd t) 
  | _ -> failwith "Something has gone wrong in score"


let current_board t = failwith "Unimplemented" 

let check t = failwith "Unimplemented" 

let checkmate t = failwith "Unimplemented"

let score game = 
  failwith "unimplemented"

let assert_valid_positions pos1 pos2  = 
  if String.length pos1 = 2 && String.length pos2 = 2 
  then 
    let l1 = String.sub pos1 0 1 in 
    let n1 = String.sub pos1 1 1 |> int_of_string in 
    let l2 = String.sub pos2 0 1 in 
    let n2 = String.sub pos2 1 1 |> int_of_string in 
  List.mem l1 
    ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "a"; "b"; "c"; "d"; "e";"f"; "g"; "h"] &&
  List.mem l2
    ["A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "a"; "b"; "c"; "d"; "e";"f"; "g"; "h"] &&
  n1 > 0 && n2 > 0 && n1 < 9 && n2 < 9 else false

(* Implement calculate score to find the pieces that are missing from the 
current board and give a score for each  *)
let calculate_score state = [("Austin", 0); ("Justin", 100000000)]


(* Find all of the missing pieces on the board for a player number (1 or 2) *)
let find_missing_pieces state player_number = 
[("ok", 100)]

let move_piece state pos = 
  match String.split_on_char ' ' pos with 
  | [] -> raise InvalidCommand 
  | h::[] -> raise InvalidCommand 
  | h::t::[] -> if assert_valid_positions h t then 
       let () =  Chessboard.move_piece state.current_board h t in 
       let new_score = calculate_score state in 
       let p1_pieces = find_missing_pieces state 1 in 
       let p2_pieces = find_missing_pieces state 2 in 
      {
        player_turn= 
          List.filter (fun x -> x <> state.player_turn) state.players |> List.hd;
        players = state.players;
        score = new_score;
        current_board = state.current_board;
        p1_taken_pieces = p1_pieces;
        p2_taken_pieces = p2_pieces;
      }
   else raise InvalidCommand
  | _ -> raise InvalidCommand

type person = string

type t = {
  player_turn: person;
  score: (person*int) list;
  current_board: Chessboard.t;
  previous_board: Chessboard.t
}
let init_state person1 person2 = failwith "Unimplemented"

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
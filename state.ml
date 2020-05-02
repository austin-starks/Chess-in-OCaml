type person = string

type t = {
  player_turn: person;
  score: person*int list;
  current_board: Chessboard.t;
  previous_board: Chessboard.t
}

let init_state person1 person2 = failwith "Unimplemented"

let turn t = failwith "Unimplemented" 

let player_name = failwith "Unimplemented" 

let score t person = failwith "Unimplemented" 

let current_board t = failwith "Unimplemented" 

let check t = failwith "Unimplemented" 

let checkmate t = failwith "Unimplemented"

let score game = 
  failwith "unimplemented"
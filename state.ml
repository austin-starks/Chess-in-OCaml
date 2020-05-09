type person = string
type piece = string 
type num_piece = int

exception InvalidCommand
exception P1Checkmate
exception P2Checkmate

type t = {
  player_turn: person;
  players: person list;
  score: (person*int) list;
  current_board: Chessboard.t;
}

let init_state person1 person2 = 
  {
    player_turn= person1;
    players= [person1; person2];
    score= [(person1, 0); (person2, 0)];
    current_board= Chessboard.initialize_chessboard ();
  }

let turn_player_name t =
  t.player_turn


let current_board t = t.current_board

let score t = 
  match t.players with 
  | [] -> failwith "There should be exactly two players"
  | p1::p2::[] -> 
    let p1_score = t.score 
                   |> List.assoc p1 
                   |> string_of_int in
    let p2_score = t.score 
                   |> List.assoc p2
                   |> string_of_int in
    if int_of_string p1_score > 1000000000 then raise P1Checkmate;
    if int_of_string p2_score > 1000000000 then raise P2Checkmate;

    p1 ^ " has " ^ p1_score ^ " points\n"^ p2 ^ " has " ^ p2_score ^ " points.\n";
  | _ -> failwith "There should be exactly two players"


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

(* Find all of the missing pieces on the board for a player number (1 or 2) *)
let find_missing_pieces state color = 
  let list_of_all_pieces =  [
    ("King", 1);
    ("Queen", 1);
    ("Rook", 2);
    ("Bishop", 2);
    ("Knight", 2);
    ("Pawn", 8);
  ] in 
  let list_of_current_pieces = Chessboard.count_pieces state.current_board color in
  let list_of_differences = 
    List.map2 (fun elt1 elt2 -> (fst elt1, snd elt1 - snd elt2)) 
      list_of_all_pieces list_of_current_pieces in 
  List.filter (fun x -> snd x <> 0) list_of_differences

let piece_score tup = 
  match fst tup with 
  | "King" -> 1000000000
  | "Pawn" -> 1 
  | "Knight" -> 3 
  | "Bishop" -> 3 
  | "Rook" -> 5 
  | "Queen" -> 9
  | _ -> failwith "Not a piece"

(* Implement calculate score to find the pieces that are missing from the 
   current board and give a score for each  *)
let calculate_score state = 
  match state.players with 
  | [] -> failwith "There should be exactly two players"
  | p1::p2::[] -> 
    let p1_miss_pieces = find_missing_pieces state "black" in 
    let p2_miss_pieces = find_missing_pieces state "white" in 
    let p1_score = List.fold_left (fun x y -> x + piece_score y) 0 p1_miss_pieces in 
    let p2_score = List.fold_left (fun x y -> x + piece_score y) 0 p2_miss_pieces in 
    [(p1, p1_score); (p2, p2_score)]
  | _ -> failwith "There should be exactly two players"


let exchange_pawns chessboard = 
  Chessboard.exchange_pawns chessboard



let move_piece state pos = 
  match String.split_on_char ' ' pos |> List.filter (fun x -> x <> "") with 
  | [] -> raise InvalidCommand 
  | h::[] -> raise InvalidCommand 
  | h::t::[] -> if assert_valid_positions h t then 
      let () =  Chessboard.move_piece state.current_board h t in 
      let () = exchange_pawns state.current_board in 
      let new_score = calculate_score state in 
      {
        player_turn= 
          List.filter (fun x -> x <> state.player_turn) state.players |> List.hd;
        players = state.players;
        score = new_score;
        current_board = state.current_board;
      }
    else raise InvalidCommand
  | _ -> raise InvalidCommand

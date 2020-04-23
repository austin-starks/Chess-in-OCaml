type color = Black | White

type piece = 
  | Pawn of color 
  | Knight of color
  | Bishop of color
  | King of color 
  | Queen of color 
  | Rook of color 
  | None

type t = piece array list

(* Internal representation of a position *)
type position = {
  letter : string;
  number: int
}

exception IllegalMoveError 

exception NoPiecePresentError 

exception SameColorMoveError

exception NotAPiece


let initialize_chessboard =  [
  [|Rook Black; Knight Black; Bishop Black; Queen Black; 
    King Black; Bishop Black; Knight Black; Rook Black|];
  [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
    Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|Pawn White; Pawn White; Pawn White; Pawn White; 
    Pawn White; Pawn White; Pawn White; Pawn White|];
  [|Rook White; Knight White; Bishop White; Queen White; 
    King White; Bishop White; Knight White; Rook White|];
]
let parse_position pos = {
  letter = String.sub pos 0 1;
  number = String.sub pos 1 1 |> int_of_string
} 

(** [pos_letter_assoc_list] is an association list that maps positions to
    numbers *)
let pos_letter_assoc_list = 
  [
    ("A", 1);  ("B", 2);  ("C", 3);  ("D", 4);  
    ("E", 5);  ("F", 6);  ("G", 7);  ("H", 8); 
    ("a", 1);  ("b", 2);  ("c", 3);  ("d", 4);  
    ("e", 5);  ("f", 6);  ("g", 7);  ("e", 8); 
  ]

(** [number_to_letter_pos_assoc_list] is an association list that maps numbers 
    to positions *)
let number_to_letter_pos_assoc_list = 
  [
    (1 ,"A");  (2 ,"B");  (3 ,"C");  (4 ,"D");  
    (5 ,"E");  (6 ,"F");  (7 ,"G");  (8 ,"H"); 
  ]

let get_chess_row t pos = 
  let rec get_row_helper t position = 
    match t, position with 
    | [], _ -> failwith "Invalid chessboard"
    | h::_, {letter = _; number = 8} -> h  
    | _::t, {letter = x; number = y} -> get_row_helper t 
                                          {letter = x; number = y+1}  in 
  get_row_helper t pos 

(** [is_rook_move p pos1 pos2] determines if the attempted move of a rook is 
    legal *)  
let is_rook_move piece pos1 pos2 =
  if (pos1.letter = pos2.letter && pos1.number <> pos2.number) 
  || (pos1.number = pos2.number && pos1.letter <> pos2.letter) 
  then true else false

(** [is_bishop_move p pos1 pos2] determines if the attempted move of a bishop is 
    legal *) 
let is_bishop_move piece pos1 pos2 = 
  let pos_number_differnece = pos2.number - pos1.number in 
  let pos_letter_difference = List.assoc pos2.letter pos_letter_assoc_list -
                              List.assoc pos1.letter pos_letter_assoc_list in
  if Int.abs pos_number_differnece = Int.abs pos_letter_difference 
  && pos_number_differnece <> 0 then true else false

(** [check_piece_color piece_to_move piece_at_loc] takes in two pieces
    and checks to see if there is a piece of the same color at that position. 
    If there's not, it return a unit. Otherwise, raises SameColorMoveError *)
let check_piece_color piece_to_move piece_at_loc = 
  match piece_to_move, piece_at_loc with 
  | None, _ -> raise NotAPiece
  | _, None -> () 
  | Rook c1, Rook c2 -> if c1 = c2 then raise SameColorMoveError
  | Rook c1, Knight c2 -> if c1 = c2 then raise SameColorMoveError
  | Rook c1, Bishop c2 -> if c1 = c2 then raise SameColorMoveError
  | Rook c1, King c2 -> if c1 = c2 then raise SameColorMoveError
  | Rook c1, Queen c2 -> if c1 = c2 then raise SameColorMoveError
  | Rook c1, Pawn c2 -> if c1 = c2 then raise SameColorMoveError
  | Knight c1, Rook c2 -> if c1 = c2 then raise SameColorMoveError
  | Knight c1, Knight c2 -> if c1 = c2 then raise SameColorMoveError
  | Knight c1, Bishop c2 -> if c1 = c2 then raise SameColorMoveError
  | Knight c1, King c2 -> if c1 = c2 then raise SameColorMoveError
  | Knight c1, Queen c2 -> if c1 = c2 then raise SameColorMoveError
  | Knight c1, Pawn c2 -> if c1 = c2 then raise SameColorMoveError  
  | Bishop c1, Rook c2 -> if c1 = c2 then raise SameColorMoveError
  | Bishop c1, Knight c2 -> if c1 = c2 then raise SameColorMoveError
  | Bishop c1, Bishop c2 -> if c1 = c2 then raise SameColorMoveError
  | Bishop c1, King c2 -> if c1 = c2 then raise SameColorMoveError
  | Bishop c1, Queen c2 -> if c1 = c2 then raise SameColorMoveError
  | Bishop c1, Pawn c2 -> if c1 = c2 then raise SameColorMoveError
  | King c1, Rook c2 -> if c1 = c2 then raise SameColorMoveError
  | King c1, Knight c2 -> if c1 = c2 then raise SameColorMoveError
  | King c1, Bishop c2 -> if c1 = c2 then raise SameColorMoveError
  | King c1, King c2 -> if c1 = c2 then raise SameColorMoveError
  | King c1, Queen c2 -> if c1 = c2 then raise SameColorMoveError
  | King c1, Pawn c2 -> if c1 = c2 then raise SameColorMoveError  
  | Queen c1, Rook c2 -> if c1 = c2 then raise SameColorMoveError
  | Queen c1, Knight c2 -> if c1 = c2 then raise SameColorMoveError
  | Queen c1, Bishop c2 -> if c1 = c2 then raise SameColorMoveError
  | Queen c1, King c2 -> if c1 = c2 then raise SameColorMoveError
  | Queen c1, Queen c2 -> if c1 = c2 then raise SameColorMoveError
  | Queen c1, Pawn c2 -> if c1 = c2 then raise SameColorMoveError
  | Pawn c1, Rook c2 -> if c1 = c2 then raise SameColorMoveError
  | Pawn c1, Knight c2 -> if c1 = c2 then raise SameColorMoveError
  | Pawn c1, Bishop c2 -> if c1 = c2 then raise SameColorMoveError
  | Pawn c1, King c2 -> if c1 = c2 then raise SameColorMoveError
  | Pawn c1, Queen c2 -> if c1 = c2 then raise SameColorMoveError
  | Pawn c1, Pawn c2 -> if c1 = c2 then raise SameColorMoveError

(** Gets the piece at a given position  *)
let get_piece t position = 
  let chess_row = get_chess_row t position in 
  let ind = List.assoc position.letter pos_letter_assoc_list - 1 in
  chess_row.(ind)

let rec bishop_path t start_pos end_pos = 
  if start_pos.number = end_pos.number && start_pos.letter = end_pos.letter 
  then true 
  else if start_pos.number < end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list < 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list + 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    (get_piece t end_pos) = None && 
    bishop_path t {number = start_pos.number + 1; letter = new_letter} end_pos
  else if start_pos.number < end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list > 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    (get_piece t end_pos) = None && 
    bishop_path t {number = start_pos.number + 1; letter = new_letter} end_pos
  else if start_pos.number > end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list < 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list + 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    (get_piece t end_pos) = None && 
    bishop_path t {number = start_pos.number - 1; letter = new_letter} end_pos
  else if start_pos.number > end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list > 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    (get_piece t end_pos) = None && 
    bishop_path t {number = start_pos.number - 1; letter = new_letter} end_pos
  else false

let rec rook_path t start_pos end_pos = 
  if end_pos.number = start_pos.number || end_pos.letter = start_pos.letter then true else false
(* then false else 
   let is_piece_blocking = get_piece t  *)

(** Checks to see if the path from one position to another is not blocked
    with another piece. If it is blocked, returns true. Otherwise returns false *)
let path_is_blocked t start_pos end_pos = 
  let piece_to_move = get_piece t start_pos in
  match piece_to_move with 
  | None -> raise NotAPiece
  | Pawn _ ->  if get_piece t end_pos = None then false else true
  | Knight _ -> false
  | Bishop _ -> bishop_path t start_pos end_pos
  | Queen _ -> failwith "Unimplemented"
  | King _ -> false
  | Rook _ -> failwith "Unimplemented"


let is_valid_move t piece pos1 pos2 = 
  if (pos1.number > 8 || pos1.number < 1 || pos2.number > 8 || pos2.number < 1)
  || path_is_blocked t pos1 pos2 
  then false else match piece with 
    | Pawn Black-> if pos1.letter = pos2.letter && (
        (pos2.number = pos1.number -1)
        || (pos1.number = 7 && pos2.number = 5)) then true else false
    | Pawn White -> if pos2.letter = pos1.letter && (
        (pos2.number = pos1.number +1)
        || (pos1.number = 2 && pos2.number = 4)) then true else false
    | Knight _ -> if (pos1.number = pos2.number +2 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list + 1) ||
                     (pos1.number = pos2.number +2 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list - 1) ||
                     (pos1.number = pos2.number -2 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list + 1) ||
                     (pos1.number = pos2.number -2 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list - 1) ||
                     (pos1.number = pos2.number +1 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list + 2) ||
                     (pos1.number = pos2.number +1 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list - 2) ||
                     (pos1.number = pos2.number - 1 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list + 2) ||
                     (pos1.number = pos2.number -1 && 
                      List.assoc pos1.letter pos_letter_assoc_list  = 
                      List.assoc pos2.letter pos_letter_assoc_list + 2) 
      then true else false 
    | Bishop _ -> is_bishop_move piece pos1 pos2
    | King _ -> (is_bishop_move piece pos1 pos2 || is_rook_move piece pos1 pos2)
                && List.assoc pos1.letter pos_letter_assoc_list  - 
                   List.assoc pos2.letter pos_letter_assoc_list |> Int.abs <=1
                && pos1.number - pos2.number |> Int.abs <=1
    | Queen _ -> is_bishop_move piece pos1 pos2 || is_rook_move piece pos1 pos2
    | Rook _ -> is_rook_move piece pos1 pos2
    | None -> false


let move_piece t pos1 pos2 = 
  let position1 = parse_position pos1 in  
  let position2 = parse_position pos2 in  
  let piece_to_move = 
    get_piece t position1 in
  (* TODO - say is_valid_move || is_valid_pawn_move *)
  if is_valid_move t piece_to_move position1 position2 
  then
    let pos1_letter_index = 
      List.assoc position1.letter pos_letter_assoc_list - 1 in
    let chess_row_pos1 = get_chess_row t position1 in 
    let chess_row_pos2 = get_chess_row t position2 in 
    let pos2_letter_index = 
      List.assoc position2.letter pos_letter_assoc_list - 1 in
    let piece_at_loc = chess_row_pos2.(pos2_letter_index) in
    let _ = check_piece_color piece_to_move piece_at_loc in 
    chess_row_pos2.(pos2_letter_index) <- piece_to_move;
    chess_row_pos1.(pos1_letter_index) <- None; 
  else raise IllegalMoveError

let get_piece_from_string str = 
  match String.lowercase_ascii str with 
  | "rook black" -> Rook Black
  | "rook white" -> Rook White
  | "pawn black" -> Pawn Black
  | "pawn white" -> Pawn White
  | "bishop black" -> Bishop Black 
  | "bishop white" -> Bishop White
  | "knight black" -> Knight Black 
  | "knight white" -> Knight White
  | "queen black" -> Queen Black  
  | "queen white" -> Queen White  
  | "king black" -> King Black 
  | "king white" -> King White 
  | _ -> raise NotAPiece

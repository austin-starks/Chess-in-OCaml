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


let initialize_chessboard = [
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

(** [parse_position pos] is a record representing the string pos
    Requires: pos to be a1, a2, ... a8, b1, ..., h7, or h8  *)
let parse_position pos = {
  letter = String.sub pos 0 1;
  number = String.sub pos 1 1 |> int_of_string
} 

(** [pos_letter_assoc_list] is an association list that maps positions to
    numbers *)
let pos_letter_assoc_list = 
  [("A", 1);  ("B", 2);  ("C", 3);  ("D", 4);  
   ("E", 5);  ("F", 6);  ("G", 7);  ("H", 8); ]


let get_chess_row t pos = 
  let rec get_row_helper t position = 
    match t, position with 
    | [], _ -> failwith "Invalid chessboard"
    | h::_, {letter = _; number = 8} -> h  
    | _::t, {letter = x; number = y} -> get_row_helper t 
                                          {letter = x; number = y+1}  in 
  get_row_helper t pos 


let is_rook_move piece pos1 pos2 =
  if (pos1.letter = pos2.letter && pos1.number <> pos2.number) 
  || (pos1.number = pos2.number && pos1.letter <> pos2.letter) 
  then true else false


let is_bishop_move piece pos1 pos2 = 
  let pos_number_differnece = pos2.number - pos1.number in 
  let pos_letter_difference = List.assoc pos2.letter pos_letter_assoc_list -
                              List.assoc pos1.letter pos_letter_assoc_list in
  if Int.abs pos_number_differnece = Int.abs pos_letter_difference 
  && pos_number_differnece <> 0 then true else false


let is_valid_move piece pos1 pos2 = 
  match piece with 
  | Pawn Black-> if pos1.letter = pos2.letter && (
      (pos1.number = pos2.number -1)
      || (pos1.number = 7 && pos2.number = 5)) then true else false
  | Pawn White -> if pos1.letter = pos2.letter && (
      (pos1.number = pos2.number +1)
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
                    List.assoc pos2.letter pos_letter_assoc_list + 2) ||
                   (pos1.number = pos2.number + 1 && 
                    List.assoc pos1.letter pos_letter_assoc_list  = 
                    List.assoc pos2.letter pos_letter_assoc_list + 2) ||
                   (pos1.number = pos2.number +1 && 
                    List.assoc pos1.letter pos_letter_assoc_list  = 
                    List.assoc pos2.letter pos_letter_assoc_list + 2) 
    then true else false 
  | Bishop _ ->  is_bishop_move piece pos1 pos2
  | King _ -> if pos2.number - pos1.number |> Int.abs = 1 || 
                 List.assoc pos2.letter pos_letter_assoc_list -
                 List.assoc pos1.letter pos_letter_assoc_list  |> Int.abs = 1 
    then true else false
  | Queen _ -> is_bishop_move piece pos1 pos2 || is_rook_move piece pos1 pos2
  | Rook _ -> is_rook_move piece pos1 pos2
  | None -> false


(** [check_piece_color piece_to_move piece_at_loc] takes in two pieces
    and checks to see if there is a piece of the same color at that position. 
    If there's not, it return a unit. Otherwise, raises SameColorMoveError *)
let check_piece_color piece_to_move piece_at_loc = 
  match piece_to_move, piece_at_loc with 
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
  | _ -> raise IllegalMoveError


let move_piece t pos1 pos2 = 
  let position1 = parse_position pos1 in  
  let position2 = parse_position pos2 in  
  let chess_row_pos1 = get_chess_row t position1 in 
  let pos1_letter_index = List.assoc position1.letter pos_letter_assoc_list - 1 in
  let piece_to_move = 
    chess_row_pos1.(pos1_letter_index) in
  if is_valid_move piece_to_move position1 position2 
  then
    let chess_row_pos2 = get_chess_row t position2 in 
    let pos2_letter_index = 
      List.assoc position2.letter pos_letter_assoc_list - 1 in
    let piece_at_loc = chess_row_pos2.(pos2_letter_index) in
    let _ = check_piece_color piece_to_move piece_at_loc in 
    chess_row_pos2.(pos2_letter_index) <- piece_to_move;
    chess_row_pos1.(pos1_letter_index) <- None; 
    List.fold_left (fun lst arr -> (Array.copy arr)::lst) [] t
  else raise IllegalMoveError
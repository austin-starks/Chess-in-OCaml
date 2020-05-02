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


let initialize_chessboard () =  [
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
    ("e", 5);  ("f", 6);  ("g", 7);  ("h", 8); 
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

let print_pos name pos = 
  print_string "\n";
  print_string "\n";
  print_string name; 
  print_string pos.letter;
  print_int pos.number;
  ()


(* current_pos: d4
   target_position: g7 *)
let rec bishop_path t start_pos end_pos = 
  (* print_pos "start_pos\n" start_pos; *)
  (* print_pos "end_pos\n" end_pos; *)
  if start_pos = end_pos 
  then false 
  (* up right: up because number is less, left because letter is less *)
  else if start_pos.number < end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list < 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    (* let () = print_endline "check 1" in *)
    let ind_new_letter = (List.assoc start_pos.letter pos_letter_assoc_list) + 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number + 1; letter = new_letter} in 
    (* print_pos "new_pos\n" new_pos; *)
    (* if (get_piece t new_pos) = None then print_endline "piece: None" else print_endline "piece: not Nine"; *)
    if ((get_piece t new_pos) <> None && new_pos <> end_pos) then true else
      bishop_path t new_pos end_pos
      (* up left down because number is less, right because letter is greater *)
  else if start_pos.number < end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list > 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    (* let () = print_endline "check 2" in *)
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number + 1; letter = new_letter} in
    (* if (get_piece t new_pos) = None then print_endline "piece: None" else print_endline "piece: not None"; *)
    (* print_pos "new_pos\n" new_pos; *)
    if ((get_piece t new_pos) <> None && new_pos <> end_pos) then true else
      bishop_path t new_pos end_pos

  (* down right left *)
  else if start_pos.number > end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list < 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    (* let () = print_endline "check 3" in *)
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list + 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number - 1; letter = new_letter} in

    if ((get_piece t new_pos) <> None && new_pos <> end_pos) then true else
      bishop_path t new_pos end_pos

  (* down left *)
  else if start_pos.number > end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list > 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    (* let () = print_endline "check 4" in *)
    let ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
    let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number - 1; letter = new_letter} in
    if ((get_piece t new_pos) <> None && new_pos <> end_pos) then true else
      bishop_path t new_pos end_pos
  else 
    (* let () = print_endline "check 5" in *)
    false

let rec rook_path_blocked t start_pos end_pos = 
  if end_pos.number = start_pos.number && end_pos.letter = start_pos.letter 
  then false else 
  if start_pos.letter = end_pos.letter then 
    if end_pos.number > start_pos.number then
      let intermediate_position = 
        {letter = start_pos.letter; number = start_pos.number+1} in
      if get_piece t intermediate_position <> None  
      && intermediate_position <> end_pos then true 
      else rook_path_blocked t intermediate_position end_pos 
    else let intermediate_position = 
           {letter = start_pos.letter; number = start_pos.number-1} in
      if get_piece t intermediate_position <> None 
      && intermediate_position <> end_pos then true 
      else rook_path_blocked t intermediate_position end_pos 
  else if start_pos.number = end_pos.number then 
    if List.assoc end_pos.letter pos_letter_assoc_list >
       List.assoc start_pos.letter pos_letter_assoc_list then let 
      ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list + 1 in 
      let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
      let intermediate_position = {letter = new_letter; number = start_pos.number} in
      if get_piece t intermediate_position <> None
      && intermediate_position <> end_pos then true 
      else 
        let () = print_endline (start_pos.letter ^ ": old letter" )in 
        let () = print_endline (intermediate_position.letter ^ ": new letter" )in 
        rook_path_blocked t intermediate_position end_pos 
    else let 
      ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
      let new_letter = List.assoc ind_new_letter number_to_letter_pos_assoc_list in
      let intermediate_position = {letter = new_letter; number = start_pos.number} in
      if get_piece t intermediate_position <> None 
      && intermediate_position <> end_pos then true 
      else rook_path_blocked t intermediate_position end_pos 
  else false

(** Checks to see if the path from one position to another is not blocked
    with another piece. If it is blocked, returns true. Otherwise returns false *)
let path_is_blocked t piece start_pos end_pos = 
  match piece with 
  | None -> print_endline (start_pos.letter ^ (start_pos.number |> string_of_int)); raise NotAPiece
  | Pawn color ->  false
  (* if (get_piece t end_pos = None && 
                       abs (end_pos.number - start_pos.number) = 1) ||
                      (abs (end_pos.number - start_pos.number) = 2 &&
                       match color with 
                       | Black -> get_piece t {letter=end_pos.letter; number=6} = None
                       | White -> get_piece t {letter=end_pos.letter; number=3} = None) 
     then false else true *)
  | Knight _ -> false
  | Bishop _ -> bishop_path t start_pos end_pos
  | Queen _ -> bishop_path t start_pos end_pos || rook_path_blocked t start_pos end_pos
  (* rook_path_blocked t start_pos end_pos || bishop_path t start_pos end_pos *)
  | King _ -> false
  | Rook _ -> rook_path_blocked t start_pos end_pos


let is_valid_move t piece pos1 pos2 = 
  if (pos1.number > 8 || pos1.number < 1 || pos2.number > 8 || pos2.number < 1)
  || path_is_blocked t piece pos1 pos2 
  then false else match piece with 
    | Pawn Black-> if pos1.letter = pos2.letter && (
        (pos2.number = pos1.number -1)
        || (pos1.number = 7 && pos2.number = 5)) ||
                      (pos2.number = pos1.number -1 && 
                       List.assoc pos2.letter pos_letter_assoc_list - 
                       List.assoc pos1.letter pos_letter_assoc_list |> Int.abs =1 &&
                       match get_piece t pos2 with 
                       | Pawn White -> true 
                       | King White -> true 
                       | Knight White ->true
                       | Bishop White -> true 
                       | Queen White -> true
                       | _ -> false)
      then true else false
    | Pawn White -> if pos2.letter = pos1.letter && (
        (pos2.number = pos1.number +1) ||
        (pos1.number = 2 && pos2.number = 4)) || 
                       (pos2.number = pos1.number +1 && 
                        List.assoc pos2.letter pos_letter_assoc_list - 
                        List.assoc pos1.letter pos_letter_assoc_list |> Int.abs =1 &&
                        match get_piece t pos2 with 
                        | Pawn Black -> true 
                        | King Black -> true 
                        | Knight Black ->true
                        | Bishop Black -> true 
                        | Queen Black -> true
                        | _ -> false)
      then true else false
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

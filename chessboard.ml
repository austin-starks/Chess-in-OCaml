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

type position = {
  letter : string;
  number: int
}

exception IllegalMoveError 

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

(** [get_chess_row t pos] is the row of the chessboard associated with position
    [pos] *)
let get_chess_row t pos = 
  let rec get_row_helper t position = 
    match t, position with 
    | [], _ -> failwith "Invalid chessboard"
    | h::_, {letter = _; number = 8} -> h  
    | _::t, {letter = x; number = y} -> get_row_helper t 
                                          {letter = x; number = y+1}  in 
  get_row_helper t pos 

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

let get_piece t position = 
  let chess_row = get_chess_row t position in 
  let ind = List.assoc position.letter pos_letter_assoc_list - 1 in
  chess_row.(ind)

(** [print_pos name pos] is a helper method used for debugging that prints 
    position [pos] *)
let print_pos name pos = 
  print_string "\n";
  print_string "\n";
  print_string name; 
  print_string pos.letter;
  print_int pos.number;
  ()

(** [bishop_path_blocked t start_pos end_pos] is a helper function for 
    [path_is_blocked] for bishops that checks to see if the path from start_pos 
    to end_pos is blocked with another piece. *)
let rec bishop_path_blocked t start_pos end_pos = 
  if end_pos.number = start_pos.number  &&  
     (end_pos.letter |> String.lowercase_ascii) = 
     (start_pos.letter |> String.lowercase_ascii)
  then false 
  else if start_pos.number < end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list < 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = 
      (List.assoc start_pos.letter pos_letter_assoc_list) + 1 in 
    let new_letter = 
      List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number + 1; letter = new_letter} in 
    if (get_piece t new_pos) = None then bishop_path_blocked t new_pos end_pos
    else not (new_pos.number = end_pos.number && 
              (new_pos.letter |> String.lowercase_ascii) = 
              (end_pos.letter |> String.lowercase_ascii))
  else if start_pos.number < end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list > 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = 
      List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
    let new_letter = 
      List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number + 1; letter = new_letter} in
    if (get_piece t new_pos) = None then bishop_path_blocked t new_pos end_pos
    else not (new_pos.number = end_pos.number && 
              (new_pos.letter |> String.lowercase_ascii) = 
              (end_pos.letter |> String.lowercase_ascii))
  else if start_pos.number > end_pos.number && 
          List.assoc start_pos.letter pos_letter_assoc_list < 
          List.assoc end_pos.letter pos_letter_assoc_list
  then 
    let ind_new_letter = 
      List.assoc start_pos.letter pos_letter_assoc_list + 1 in 
    let new_letter = 
      List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number - 1; letter = new_letter} in
    if (get_piece t new_pos) = None then bishop_path_blocked t new_pos end_pos
    else not (new_pos.number = end_pos.number && 
              (new_pos.letter |> String.lowercase_ascii) = 
              (end_pos.letter |> String.lowercase_ascii))
  else if not (start_pos.number > end_pos.number && 
               List.assoc start_pos.letter pos_letter_assoc_list > 
               List.assoc end_pos.letter pos_letter_assoc_list)
  then 
    let ind_new_letter = 
      List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
    let new_letter = 
      List.assoc ind_new_letter number_to_letter_pos_assoc_list in
    let new_pos = {number = start_pos.number - 1; letter = new_letter} in
    if (get_piece t new_pos) = None then bishop_path_blocked t new_pos end_pos
    else not (new_pos.number = end_pos.number && 
              (new_pos.letter |> String.lowercase_ascii) = 
              (end_pos.letter |> String.lowercase_ascii))
  else 
    false

(** [rook_path_blocked t start_pos end_pos] is a helper function for 
    [path_is_blocked] for rooks that checks to see if the path from start_pos to 
    end_pos is blocked with another piece. *)
let rec rook_path_blocked t start_pos end_pos = 
  if end_pos.number = start_pos.number && 
     (end_pos.letter |> String.lowercase_ascii) = 
     (start_pos.letter |> String.lowercase_ascii)
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
      if get_piece t intermediate_position <> None then true 
      else rook_path_blocked t intermediate_position end_pos 
  else if start_pos.number = end_pos.number then 
    if List.assoc end_pos.letter pos_letter_assoc_list >
       List.assoc start_pos.letter pos_letter_assoc_list then let 
      ind_new_letter = List.assoc start_pos.letter pos_letter_assoc_list + 1 in 
      let new_letter = 
        List.assoc ind_new_letter number_to_letter_pos_assoc_list in
      let intermediate_position = 
        {letter = new_letter; number = start_pos.number} in
      if get_piece t intermediate_position <> None && 
         end_pos.number <> intermediate_position.number &&
         String.lowercase_ascii end_pos.letter <> 
         String.lowercase_ascii intermediate_position.letter 
      then 
        true
      else 
        rook_path_blocked t intermediate_position end_pos 
    else 
      let ind_new_letter = 
        List.assoc start_pos.letter pos_letter_assoc_list - 1 in 
      let new_letter = 
        List.assoc ind_new_letter number_to_letter_pos_assoc_list in
      let intermediate_position = 
        {letter = new_letter; number = start_pos.number} in
      if get_piece t intermediate_position <> None && 
         end_pos.number <> intermediate_position.number &&
         String.lowercase_ascii end_pos.letter <> 
         String.lowercase_ascii intermediate_position.letter 
      then  true 
      else rook_path_blocked t intermediate_position end_pos 
  else false

(** [path_is_blocked t piece start_pos end_pos] checks to see if the path from 
    one position to another is not blocked with another piece. If it is blocked, 
    returns true. Otherwise returns false *)
let path_is_blocked t piece start_pos end_pos = 
  match piece with 
  | None -> raise NotAPiece
  | Pawn color ->  if 
    (start_pos.letter = end_pos.letter && get_piece t end_pos <> None) 
    then true else false
  | Knight _ -> false
  | Bishop _ -> bishop_path_blocked t start_pos end_pos
  | Queen _ -> 
    if (start_pos.number = end_pos.number
        || start_pos.letter |> String.lowercase_ascii = 
                               (end_pos.letter |> String.lowercase_ascii)) 
    then rook_path_blocked t start_pos end_pos 
    else bishop_path_blocked t start_pos end_pos
  | King _ -> false
  | Rook _ -> rook_path_blocked t start_pos end_pos

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


let is_valid_move t piece pos1 pos2 = 
  if (pos1.number > 8 || pos1.number < 1 || pos2.number > 8 || pos2.number < 1)
  then false else match piece with 
    | Pawn Black-> if 
      (
        (pos2.number = pos1.number -1 || pos1.number = 7 && pos2.number = 5) &&
        (pos2.letter = pos1.letter)
      ) 
      ||
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
    | Pawn White -> if 
      (
        (pos2.number = pos1.number +1 || pos1.number = 2 && pos2.number = 4)  && 
        (pos2.letter = pos1.letter)
      ) || 
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
    | King color -> ((is_bishop_move piece pos1 pos2 || 
                      is_rook_move piece pos1 pos2)
                     && List.assoc pos1.letter pos_letter_assoc_list  - 
                        List.assoc pos2.letter pos_letter_assoc_list 
                        |> Int.abs <=1
                     && pos1.number - pos2.number |> Int.abs <=1) 
                    || 
                    (get_piece t pos1 = King color && 
                     get_piece t {letter = "F"; number = 1} = None && 
                     get_piece t {letter = "G"; number = 1} = None && 
                     get_piece t {letter = "H"; number = 1} = Rook color &&
                     List.assoc pos1.letter pos_letter_assoc_list  = 
                     List.assoc pos2.letter pos_letter_assoc_list - 2
                    ) 
                    ||
                    (get_piece t pos1 = King color && 
                     get_piece t {letter = "F"; number = 8} = None && 
                     get_piece t {letter = "G"; number = 8} = None && 
                     get_piece t {letter = "H"; number = 8} = Rook color &&
                     List.assoc pos1.letter pos_letter_assoc_list  = 
                     List.assoc pos2.letter pos_letter_assoc_list - 2

                    ) 
                    ||
                    (get_piece t pos1 = King color && 
                     get_piece t {letter = "D"; number = 1} = None && 
                     get_piece t {letter = "C"; number = 1} = None && 
                     get_piece t {letter = "B"; number = 1} = None && 
                     get_piece t {letter = "A"; number = 1} = Rook color  &&
                     List.assoc pos1.letter pos_letter_assoc_list  = 
                     List.assoc pos2.letter pos_letter_assoc_list + 2
                    ) 
                    ||
                    (get_piece t pos1 = King color && 
                     get_piece t {letter = "D"; number = 8} = None && 
                     get_piece t {letter = "C"; number = 8} = None && 
                     get_piece t {letter = "B"; number = 8} = None && 
                     get_piece t {letter = "A"; number = 8} = Rook color &&
                     List.assoc pos1.letter pos_letter_assoc_list  = 
                     List.assoc pos2.letter pos_letter_assoc_list + 2
                    ) 

    | Queen _ -> is_bishop_move piece pos1 pos2 || is_rook_move piece pos1 pos2
    | Rook _ -> is_rook_move piece pos1 pos2
    | None -> raise NotAPiece


let move_piece t pos1 pos2 = 
  let position1 = parse_position pos1 in  
  let position2 = parse_position pos2 in  
  let piece_to_move = 
    get_piece t position1 in
  if is_valid_move t piece_to_move position1 position2 && not 
       (path_is_blocked t piece_to_move position1 position2 )
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
    match piece_to_move with 
    | King color ->  if 
      List.assoc position1.letter pos_letter_assoc_list - 
      List.assoc position2.letter pos_letter_assoc_list = 2
      then 
        let _ = chess_row_pos1.(0) <- None in 
        chess_row_pos1.(3) <- Rook color
      else if 
        List.assoc position1.letter pos_letter_assoc_list - 
        List.assoc position2.letter pos_letter_assoc_list = -2
      then 
        let _ = chess_row_pos1.(7) <- None in 
        chess_row_pos1.(5) <- Rook color
    | _ -> ()
  else raise IllegalMoveError

(** [piece_to_string piece] is the string representation of piece [piece] *)
let piece_to_string piece = 
  match piece with 
  | Rook _ -> "Rook"
  | Bishop _ -> "Bishop"
  | King _ -> "King"
  | Queen _ -> "Queen"
  | Pawn _ -> "Pawn"
  | Knight _ -> "Knight"
  | None -> ""

(** [piece_color piece] is the color of the piece [piece] (as a string). *)
let piece_color piece = 
  match piece with 
  | Rook c -> if c = White then "white" else "black"
  | Bishop c -> if c = White then "white" else "black"
  | King c -> if c = White then "white" else "black"
  | Queen c -> if c = White then "white" else "black"
  | Pawn c -> if c = White then "white" else "black"
  | Knight c -> if c = White then "white" else "black"
  | None -> "     "

(** [string_color clr] is the string representation of a Color [clr] *)
let string_color clr = match clr with 
  | Black -> "black"
  | White -> "white"

let count_pieces t color = 
  let rook_count = ref 0 in 
  let pawn_count = ref 0 in 
  let bishop_count = ref 0 in 
  let queen_count = ref 0 in 
  let king_count = ref 0 in 
  let knight_count = ref 0 in 
  List.iter (
    fun row -> Array.iter (fun piece -> match piece with 
        | Rook clr ->
          if piece_color piece = color then rook_count := !rook_count + 1
        | Pawn clr ->
          if piece_color piece = color then pawn_count := !pawn_count +  1
        | Bishop clr ->
          if piece_color piece = color then bishop_count := !bishop_count +  1
        | Knight clr ->
          if piece_color piece = color then knight_count := !knight_count +  1
        | King clr ->
          if piece_color piece = color then king_count := !king_count +  1
        | Queen clr ->
          if piece_color piece = color then queen_count := !queen_count +  1
        | None ->
          ()
      ) row) t;
  [
    ("King", !king_count);
    ("Queen", !queen_count);
    ("Rook", !rook_count);
    ("Bishop", !bishop_count);
    ("Knight", !knight_count);
    ("Pawn", !pawn_count);
  ]

(** [get_piece_from_console ()] takes in input from the console and returns the
    piece it represents (as a string) *)
let rec get_piece_from_console () = 
  print_string "> ";
  match read_line () |> String.lowercase_ascii with 
  | "bishop" -> "bishop"
  | "queen" -> "queen"
  | "king" -> "king"
  | "knight" -> "knight"
  | "rook" -> "rook"
  | _ -> print_endline "That is not a valid piece. Please pick another.";
    get_piece_from_console ()

let exchange_pawns t = 
  let exchange_pawn_helper row = 
    for x = 0 to 7 do 
      match row.(x) with 
      | Pawn clr -> 
        print_endline "Your pawn has reached the end of the board.";
        print_endline "Type in the name of a piece you want to exchange it for.";
        let piece_str = get_piece_from_console () in 
        let piece = get_piece_from_string (piece_str ^ " " ^ (string_color clr))  
        in row.(x) <- piece
      | _ -> ()
    done in
  let row1 = List.hd t in 
  let row2 = List.rev t |> List.hd in 
  exchange_pawn_helper row1;
  exchange_pawn_helper row2;;

(* [print_row ind row] prints the index of the row [ind] and all of the pieces 
    on the row. Black pieces are printed as blue. This is to ensure capatibility
    with consoles with a white/black background. *)
let print_row ind row =
  let print_extra_space piece = 
    let i = ref (String.length piece) in 
    while !i < 6 do 
      print_string " "; i := !i + 1; 
    done in
  let iter = ref 0 in 
  ANSITerminal.(print_string [red] ((string_of_int ind)^"   ")); 
  while !iter < 8 do 
    let color = piece_color (row.(!iter)) in 
    let string_piece = (row.(!iter) |> piece_to_string) in
    if color = "white" then 
      ANSITerminal.(print_string [yellow]
                      string_piece);
    if color = "black" then 
      ANSITerminal.(print_string [blue]
                      string_piece);
    print_string "  ";
    iter := !iter + 1;
    print_extra_space string_piece;
    if !iter = 8 then ANSITerminal.(print_string [red] 
                                      ((string_of_int ind)^"  \n")); 
  done 

let print_board t = 
  ANSITerminal.(print_string [red]
                  "\n     A        B       C       D       E       F       G       H\n");
  List.iter2 print_row [8;7;6;5;4;3;2;1] t;
  ANSITerminal.(print_string [red]
                  "     A        B       C       D       E       F       G       H\n\n");

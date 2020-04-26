open OUnit2
open Chessboard
(* open State *)

let board = Chessboard.initialize_chessboard

let make_is_valid_move_test 
    (name: string)
    (piece: string)
    (original_position: string)
    (new_position: string)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output  
        (is_valid_move board (piece |> get_piece_from_string) 
           (original_position |> parse_position) (new_position |> parse_position)))

let move_piece_tests = []

let is_valid_move_tests = [
  (* Pawn tests *)
  make_is_valid_move_test "Testing if white pawn works" "pawn white" 
    "E2" "E4" true;
  make_is_valid_move_test "Testing if white pawn works (1 move)" "pawn white" 
    "A2" "A3" true;
  make_is_valid_move_test "Testing if white pawn not moves diagonal" "pawn white" 
    "A2" "B3" false;
  make_is_valid_move_test "Testing if white pawn works" "pawn white" 
    "E2" "E5" false;
  make_is_valid_move_test "Testing if white pawn fails correctly" "pawn white" 
    "E3" "E5" false;
  make_is_valid_move_test "Testing if white pawn fails going backwords" "pawn white" 
    "E4" "E2" false;
  make_is_valid_move_test "Testing if black pawn works" "pawn black" 
    "C7" "C5" true;
  make_is_valid_move_test "Testing if black pawn works (1 move)" "pawn black" 
    "E7" "E6" true;
  make_is_valid_move_test "Testing if black pawn fails moving backwards" "pawn black" 
    "E2" "E4" false;
  make_is_valid_move_test "Testing if black pawn fails: not moving" "pawn black" 
    "E2" "E2" false;

  (* bishop tests *)
  make_is_valid_move_test "Testing if bishop works" "bishop black" 
    "A8" "D5" true;
  make_is_valid_move_test "Testing if bishop works 2" "bishop white" 
    "G1" "A7" true;
  make_is_valid_move_test "Testing if bishop works 3" "bishop black" 
    "H3" "G2" true;
  make_is_valid_move_test "Testing if bishop works 3" "bishop black" 
    "C5" "E3" true;
  make_is_valid_move_test "Testing if bishop fails properly" "bishop white" 
    "A5" "F7" false;
  make_is_valid_move_test "Testing if bishop fails properly: same square" 
    "bishop black" "A5" "A5" false;
  make_is_valid_move_test "Testing if bishop fails properly: non-existent move"
    "bishop white" "A9" "B8" false;

  (* rook tests *)
  make_is_valid_move_test "Testing if rook works" "rook black" 
    "A8" "A2" true;
  make_is_valid_move_test "Testing if rook works 2" "rook white" 
    "G7" "A7" true;
  make_is_valid_move_test "Testing if rook works 3" "rook black" 
    "H1" "H7" true;
  make_is_valid_move_test "Testing if rook works 3" "rook black" 
    "B3" "B4" true;
  make_is_valid_move_test "Testing if rook fails" "rook black" 
    "H1" "G7" false;
  make_is_valid_move_test "Testing if rook fails: diagonal" "rook black" 
    "D1" "C2" false;
  make_is_valid_move_test "Testing if rook: same place" "rook black" 
    "C2" "C2" false;

  (* queen tests *)
  make_is_valid_move_test "Testing if queen works" "queen black" 
    "A8" "A2" true;
  make_is_valid_move_test "Testing if queen works 2" "queen white" 
    "G7" "A7" true;
  make_is_valid_move_test "Testing if queen works 3" "queen black" 
    "H1" "H7" true;
  make_is_valid_move_test "Testing if queen works 4" "queen black" 
    "B3" "B4" true;
  make_is_valid_move_test "Testing if queen works: diagonal" "queen black" 
    "D1" "C2" true;
  make_is_valid_move_test "Testing if queen works: further move" 
    "queen black" "A7" "G1" true;
  make_is_valid_move_test "Testing if queen fails" "queen black" 
    "H1" "G7" false;
  make_is_valid_move_test "Testing if queen: same place" "queen black" 
    "C2" "C2" false;

  (* bishop tests *)
  make_is_valid_move_test "Testing if bishop works: one move" "bishop black" 
    "D1" "C2" true;
  make_is_valid_move_test "Testing if bishop works: further move" "bishop white" 
    "G8" "D5" true;
  make_is_valid_move_test "Testing if bishop works: furthest  move" 
    "bishop black" "A7" "G1" true;
  make_is_valid_move_test "Testing if bishop: same place" "bishop black" 
    "C2" "C2" false;

  (* king tests *)
  make_is_valid_move_test "Testing if king works" "king black" 
    "A8" "A7" true;
  make_is_valid_move_test "Testing if king works 2" "king white" 
    "G7" "H7" true;
  make_is_valid_move_test "Testing if king works 3" "king black" 
    "A4" "B5" true;
  make_is_valid_move_test "Testing if king works 4" "king black" 
    "H3" "G4" true;
  make_is_valid_move_test "Testing if king works: diagonal" "king black" 
    "D1" "E1" true;
  make_is_valid_move_test "Testing if king fails" "king black" "A1" "A3" false;
  make_is_valid_move_test "Testing if king fails 2" "king black" 
    "E1" "G2" false;
  make_is_valid_move_test "Testing if king fails 3" "king white" 
    "E1" "E3" false;
  make_is_valid_move_test "Testing if king: same place" "king black" 
    "C2" "C2" false;

  (* knight tests *)
  make_is_valid_move_test "Testing if knight works" "knight black" 
    "A8" "B6" true;
  make_is_valid_move_test "Testing if knight works 2" "knight white" 
    "B8" "D7" true;
  make_is_valid_move_test "Testing if knight works 3" "knight black" 
    "B8" "A6" true;
  make_is_valid_move_test "Testing if knight works 4" "knight black" 
    "D7" "B6" true;
  make_is_valid_move_test "Testing if knight works 5" "knight black" 
    "C1" "B3" true;
  make_is_valid_move_test "Testing moving knight to take queen in example" 
    "knight black" "F6" "H5" true;
  make_is_valid_move_test "Testing if knight fails" "knight black" "A1" "A3" false;
  make_is_valid_move_test "Testing if knight fails 2" "knight black" 
    "E1" "G1" false;
  make_is_valid_move_test "Testing if knight fails 3" "knight white" 
    "E1" "E3" false;
  make_is_valid_move_test "Testing if knight: same place" "knight black" 
    "C2" "C2" false;
  make_is_valid_move_test "Testing if knight: same place" "knight black" 
    "A2" "J2" false;
]

(* Testing move_piece *)
let _  = (move_piece board "E2" "E4") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

let _  = (move_piece board "E7" "E5") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; None; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; Knight White; Rook White|];
  ])

let _  = (move_piece board "G1" "F3") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; Knight Black; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; None; Rook White|];
  ]  )

let _  = (move_piece board "G8" "F6") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; Knight Black; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; None;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; Queen White; 
      King White; Bishop White; None; Rook White|];
  ]  )

let _  = (move_piece board "D1" "H5") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; Knight Black; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; Queen White;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; None; 
      King White; Bishop White; None; Rook White|];
  ]  )

let _  = (move_piece board "F6" "H5") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; Knight Black;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; None; 
      King White; Bishop White; None; Rook White|];
  ]  )

let _  = (move_piece board "F1" "E2") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|None; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; Knight Black;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      Bishop White; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; None; 
      King White; None; None; Rook White|];
  ]  )

let _  = (move_piece board "A7" "A6") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|None; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|Pawn Black; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; Knight Black;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      Bishop White; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; None; 
      King White; None; None; Rook White|];
  ]  )


let _  = (move_piece board "E2" "H5") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|None; Pawn Black; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|Pawn Black; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; Bishop White;|];
    [|None; None; None; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; Pawn White; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; None; 
      King White; None; None; Rook White|];
  ]  )

let _  = (move_piece board "B7" "B5") 
let _  = (move_piece board "C2" "C4") 
let _  = (move_piece board "B5" "C4") 

let () = assert (board = [
    [|Rook Black; Knight Black; Bishop Black; Queen Black; 
      King Black; Bishop Black; None; Rook Black|];
    [|None; None; Pawn Black; Pawn Black; 
      None; Pawn Black; Pawn Black; Pawn Black|];
    [|Pawn Black; None; None; None; None; None; None; None;|];
    [|None; None; None; None; Pawn Black; None; None; Bishop White;|];
    [|None; None; Pawn Black; None; Pawn White; None; None; None;|];
    [|None; None; None; None; None; Knight White; None; None;|];
    [|Pawn White; Pawn White; None; Pawn White; 
      None; Pawn White; Pawn White; Pawn White|];
    [|Rook White; Knight White; Bishop White; None; 
      King White; None; None; Rook White|];
  ]  )



let suite =
  "test suite for final"  >::: List.flatten [
    is_valid_move_tests
  ]

let _ = run_test_tt_main suite
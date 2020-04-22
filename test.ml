open OUnit2
open Chessboard
(* open State *)

let board = Chessboard.initialize_chessboard

let rook_board = [
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; Rook White; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
  [|None; None; None; None; None; None; None; None;|];
]

let make_is_valid_move_test 
    (name: string)
    (piece: string)
    (original_position: string)
    (new_position: string)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output  
        (is_valid_move (piece |> get_piece_from_string) 
           (original_position |> parse_position) (new_position |> parse_position)))

let move_piece_tests = []

let is_valid_move_tests = [
  (* Pawn tests *)
  make_is_valid_move_test "Testing if white pawn works" "pawn white" 
    "E2" "E4" true;
  make_is_valid_move_test "Testing if white pawn works (1 move)" "pawn white" 
    "A2" "A3" true;
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
  make_is_valid_move_test "Testing if queen works 3" "queen black" 
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

]


let suite =
  "test suite for final"  >::: List.flatten [
    is_valid_move_tests
  ]

let _ = run_test_tt_main suite
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
  make_is_valid_move_test "Testing if white pawn works" "pawn white" 
    "E2" "E4" true;
  make_is_valid_move_test "Testing if white pawn works (1 move)" "pawn white" 
    "A2" "A3" true;
  make_is_valid_move_test "Testing if white pawn works" "pawn white" 
    "E2" "E5" false;
  make_is_valid_move_test "Testing if white pawn works" "pawn white" 
    "E3" "E5" true;



]


let suite =
  "test suite for final"  >::: List.flatten [
    is_valid_move_tests
  ]

let _ = run_test_tt_main suite
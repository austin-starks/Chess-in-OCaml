open OUnit2
open Chessboard
open State

let board = Chessboard.initialize_chessboard

let make_move_piece_test
    (name: string)
    (chessboard: Chessboard.t)
    (original_position: string)
    (new_position: string)
    (expected_output : Chessboard.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output  
        (move_piece chessboard original_position new_position))


let move_piece_tests = [
  make_move_piece_test "Move pawn test" board "E2" "E4" board
]

let suite =
  "test suite for final"  >::: List.flatten [
    move_piece_tests
  ]

let _ = run_test_tt_main suite
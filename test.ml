open OUnit2
open Chessboard
open State

let board = Chessboard.initialize_chessboard

let make_get_piece_test =  
  (name: string)
    (t: t)
    (pos: string)
    (expected_output : piece) : test = 
                                name >:: (fun _ -> 
        assert_equal expected_output  (get_piece t pos))

let get_piece_tests = []

let suite =
  "test suite for final"  >::: List.flatten [
    get_piece_tests;
  ]

let _ = run_test_tt_main suite
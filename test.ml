open OUnit2
open Chessboard
(* open State *)

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
  make_move_piece_test "Move pawn test" board "E2" "E4" 
    [
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
    ]
]

let suite =
  "test suite for final"  >::: List.flatten [
    move_piece_tests
  ]

let _ = run_test_tt_main suite
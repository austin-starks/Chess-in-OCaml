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
  make_move_piece_test "Move white pawn test" board "E2" "E4" 
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
    ];
  make_move_piece_test "Move white pawn once test" board "E2" "E3" 
    [
      [|Rook Black; Knight Black; Bishop Black; Queen Black; 
        King Black; Bishop Black; Knight Black; Rook Black|];
      [|Pawn Black; Pawn Black; Pawn Black; Pawn Black; 
        Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; Pawn White; None; None; None;|];
      [|Pawn White; Pawn White; Pawn White; Pawn White; 
        None; Pawn White; Pawn White; Pawn White|];
      [|Rook White; Knight White; Bishop White; Queen White; 
        King White; Bishop White; Knight White; Rook White|];
    ];
  (* make_move_piece_test "Move black pawn once test" board "A7" "A6" 
     [
      [|Rook Black; Knight Black; Bishop Black; Queen Black; 
        King Black; Bishop Black; Knight Black; Rook Black|];
      [|None; Pawn Black; Pawn Black; Pawn Black; 
        Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
      [|Pawn Black; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|Pawn White; Pawn White; Pawn White; Pawn White; 
        Pawn White; Pawn White; Pawn White; Pawn White|];
      [|Rook White; Knight White; Bishop White; Queen White; 
        King White; Bishop White; Knight White; Rook White|];

     ]; *)

  (* make_move_piece_test "Move black pawn twice test" board "B7" "B5" 
     [
      [|Rook Black; Knight Black; Bishop Black; Queen Black; 
        King Black; Bishop Black; Knight Black; Rook Black|];
      [|Pawn Black; None; Pawn Black; Pawn Black; 
        Pawn Black; Pawn Black; Pawn Black; Pawn Black|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; Pawn Black; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; Pawn White; None; None; None;|];
      [|Pawn White; Pawn White; Pawn White; Pawn White; 
        None; Pawn White; Pawn White; Pawn White|];
      [|Rook White; Knight White; Bishop White; Queen White; 
        King White; Bishop White; Knight White; Rook White|];
     ]; *)

  make_move_piece_test "Move rook vertically up" rook_board "E4" "E7" 
    [
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; Rook White; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
    ];

  make_move_piece_test "Move rook vertically down" rook_board "E4" "E1" 
    [
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; Rook White; None; None; None;|];
    ];

  make_move_piece_test "Move rook horizontally left" rook_board "E4" "A4" 
    [
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|Rook White; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
      [|None; None; None; None; None; None; None; None;|];
    ];


]

let suite =
  "test suite for final"  >::: List.flatten [
    move_piece_tests
  ]

let _ = run_test_tt_main suite
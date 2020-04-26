open Chessboard
(* open State *)

let board = Chessboard.initialize_chessboard ()

(* Testing move white pawn *)
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

(* Testing move black pawn *)
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

(* Testing move knight *)
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


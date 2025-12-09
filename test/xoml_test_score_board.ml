open Common
open Engine
open Xoml_test_support

(* Score board *)

let test_score_board () =
    let g = init_test_board () in
    let g = apply_move g X "B8" in
    let g = apply_move g X "C7" in
    let g = apply_move g X "D6" in
    let score = Engine.score_board g X in
    (* Score is 3018 for now, but should be 1000 *)
    Alcotest.(check int) "score must be 3018" 3018 score


let suite : string * unit Alcotest.test_case list =
    "Score board",
    [
        Alcotest.test_case "Score simple board" `Quick test_score_board;
    ]


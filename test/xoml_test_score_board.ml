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
    Alcotest.(check int) "score must be 1018" 1018 score

let test_check_winner () =
    let g = init_test_board () in
    let g = apply_move g X "B8" in
    let g = apply_move g X "C7" in
    let g = apply_move g X "D6" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F4" in
    let winner = Engine.find_winner g in
    Alcotest.(check (option player_testable)) "winner must be X" (Some X) winner


let suite : string * unit Alcotest.test_case list =
    "Score board",
    [
        Alcotest.test_case "Score simple board" `Quick test_score_board;
        Alcotest.test_case "Check winner"       `Quick test_check_winner;
    ]


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

let recognize_4_in_row_threat_1 () =
    let g = init_test_board () in
    let g = Engine.apply_move g X "D7" in
    let g = Engine.apply_move g X "C6" in
    let g = Engine.apply_move g O "D6" in
    let g = Engine.apply_move g X "E6" in
    let g = Engine.apply_move g X "F6" in
    let g = Engine.apply_move g O "D5" in
    let g = Engine.apply_move g X "E5" in
    let g = Engine.apply_move g X "B4" in
    let g = Engine.apply_move g O "C4" in
    let g = Engine.apply_move g O "D4" in
    let g = Engine.apply_move g O "E4" in
    let g = Engine.apply_move g O "F4" in
    let g = Engine.apply_move g X "G4" in
    check_best_move g O "F5"

let recognize_4_in_row_threat_2 () =
    let g = init_test_board () in
    let g = Engine.apply_move g X "G7" in
    let g = Engine.apply_move g O "E6" in
    let g = Engine.apply_move g X "E5" in
    let g = Engine.apply_move g O "F5" in
    let g = Engine.apply_move g X "H5" in
    let g = Engine.apply_move g X "C4" in
    let g = Engine.apply_move g X "D4" in
    let g = Engine.apply_move g X "E4" in
    let g = Engine.apply_move g O "F4" in
    let g = Engine.apply_move g O "H4" in
    let g = Engine.apply_move g X "I4" in
    let g = Engine.apply_move g O "D3" in
    let g = Engine.apply_move g X "E3" in
    let g = Engine.apply_move g O "F3" in
    let g = Engine.apply_move g X "G3" in
    let g = Engine.apply_move g X "H3" in
    let g = Engine.apply_move g O "C2" in
    let g = Engine.apply_move g O "D2" in
    let g = Engine.apply_move g O "E2" in
    let g = Engine.apply_move g X "F2" in
    let g = Engine.apply_move g O "D1" in
    let g = Engine.apply_move g X "E1" in
    let g = Engine.apply_move g O "F1" in
    let g = Engine.apply_move g O "G1" in
    let g = Engine.apply_move g X "C0" in
    check_best_move g O "F6"

let recognize_4_in_row_threat_3 () =
    let g = init_test_board () in
    let g = Engine.apply_move g X "E5" in
    let g = Engine.apply_move g X "G3" in
    let g = Engine.apply_move g X "H2" in
    let g = Engine.apply_move g X "C7" in
    let g = Engine.apply_move g O "C6" in
    let g = Engine.apply_move g O "B5" in
    let g = Engine.apply_move g O "C5" in
    let g = Engine.apply_move g X "D5" in
    let g = Engine.apply_move g X "F5" in
    let g = Engine.apply_move g O "C4" in
    let g = Engine.apply_move g O "D4" in
    let g = Engine.apply_move g X "E4" in
    let g = Engine.apply_move g X "B3" in
    let g = Engine.apply_move g O "C3" in
    let g = Engine.apply_move g O "D3" in
    let g = Engine.apply_move g O "E3" in
    let g = Engine.apply_move g O "F3" in
    let g = Engine.apply_move g X "C2" in
    let g = Engine.apply_move g O "D2" in
    let g = Engine.apply_move g X "E2" in
    let g = Engine.apply_move g X "F2" in
    check_best_move g O "F4"

let recognize_next_move_loss () =
    let g = init_test_board () in
    let g = Engine.apply_move g X "D7" in
    let g = Engine.apply_move g O "E7" in
    let g = Engine.apply_move g X "E6" in
    let g = Engine.apply_move g O "F6" in
    let g = Engine.apply_move g X "E5" in
    let g = Engine.apply_move g X "F5" in
    let g = Engine.apply_move g O "G5" in
    let g = Engine.apply_move g X "I5" in
    let g = Engine.apply_move g O "D4" in
    let g = Engine.apply_move g X "E4" in
    let g = Engine.apply_move g X "F4" in
    let g = Engine.apply_move g X "G4" in
    let g = Engine.apply_move g X "H4" in
    let g = Engine.apply_move g O "I4" in
    let g = Engine.apply_move g X "E3" in
    let g = Engine.apply_move g O "G3" in
    let g = Engine.apply_move g O "H3" in
    let g = Engine.apply_move g O "I3" in
    let g = Engine.apply_move g X "D2" in
    let g = Engine.apply_move g O "E2" in
    let g = Engine.apply_move g O "F2" in
    let g = Engine.apply_move g O "G2" in
    let g = Engine.apply_move g X "H2" in
    let g = Engine.apply_move g O "I2" in
    let g = Engine.apply_move g X "F1" in
    let g = Engine.apply_move g O "I1" in
    let g = Engine.apply_move g X "I0" in
    check_best_move g O "C8"

let check_for_not_useless_move () =
    let g = init_test_board () in
    let g = Engine.apply_move g O "F6" in
    let g = Engine.apply_move g O "G6" in
    let g = Engine.apply_move g X "H6" in
    let g = Engine.apply_move g X "F5" in
    let g = Engine.apply_move g O "H5" in
    let g = Engine.apply_move g X "E4" in
    let g = Engine.apply_move g X "F4" in
    let g = Engine.apply_move g O "G4" in
    let g = Engine.apply_move g O "H4" in
    let g = Engine.apply_move g O "I4" in
    let g = Engine.apply_move g X "F3" in
    let g = Engine.apply_move g X "G3" in
    let g = Engine.apply_move g X "J3" in
    check_best_move_is_not g O "J0"


let suite : string * unit Alcotest.test_case list =
    "Board situations",
    [
        Alcotest.test_case "Score simple board"                 `Quick test_score_board;
        Alcotest.test_case "Check winner"                       `Quick test_check_winner;
        Alcotest.test_case "Recognize 4-in-row threat (1)"      `Quick recognize_4_in_row_threat_1;
        Alcotest.test_case "Recognize 4-in-row threat (2)"      `Quick recognize_4_in_row_threat_2;
        Alcotest.test_case "Recognize 4-in-row threat (3)"      `Quick recognize_4_in_row_threat_3;
        Alcotest.test_case "Recognize a loss on the next move"  `Quick recognize_next_move_loss;
        Alcotest.test_case "Check for a useless move"           `Quick check_for_not_useless_move;
    ]


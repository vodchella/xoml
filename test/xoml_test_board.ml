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

let recognize_4_in_row_threat () =
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
    let m = Engine.find_best_move g O  in
    match m with
    | Some i ->
        let ms = i
            |> Common.point_of_index g
            |> Option.get
            |> Common.move_str_of_point g
        in
        Alcotest.(check string) "best move must be F5" "F5" ms
    | None ->
        failwith "best move not found"


let suite : string * unit Alcotest.test_case list =
    "Score board",
    [
        Alcotest.test_case "Score simple board"         `Quick test_score_board;
        Alcotest.test_case "Check winner"               `Quick test_check_winner;
        Alcotest.test_case "Recognize 4-in-row threat"  `Quick recognize_4_in_row_threat;
    ]


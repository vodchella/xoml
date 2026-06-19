open Common
open Engine
open Patterns
open Xoml_test_support

(* Patterns *)

let check_pattern (g: game) (move_str: string) (ptrn: pattern) =
    let p        = point_of_move_str g move_str in
    let lst, res = pattern_at_point g p X ptrn in
    let dir      = string_of_direction ptrn.dir in
    let kind     = string_of_pattern_kind ptrn.kind in
    let name     = kind ^ "_" ^ dir in
    let rq_cnt   = pattern_required_points_count ptrn in
    Alcotest.(check bool) ("pattern " ^ name ^ " found") true res;
    Alcotest.(check int)  ("pattern " ^ name ^ " occupied points count")  rq_cnt (List.length lst)


let test_patterns_5 () =
    let g = init_test_board () in

    let g = apply_move g X "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    check_pattern g "B5" patterns.(0);

    let g = apply_move g X "B5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "E2" in
    let g = apply_move g X "F1" in
    check_pattern g "B5" patterns.(1);

    let g = apply_move g X "B5" in
    let g = apply_move g X "B4" in
    let g = apply_move g X "B3" in
    let g = apply_move g X "B2" in
    let g = apply_move g X "B1" in
    check_pattern g "B5" patterns.(2);

    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    let g = apply_move g X "B1" in
    check_pattern g "F5" patterns.(3)

let test_patterns_4 () =
    let g = init_test_board () in

    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    check_pattern g "C5" patterns.(4);

    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    let g = apply_move g X "F2" in
    check_pattern g "C5" patterns.(5);

    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    let g = apply_move g X "C2" in
    check_pattern g "C5" patterns.(6);

    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    check_pattern g "F5" patterns.(7);

    ()


let suite : string * unit Alcotest.test_case list =
    "Patterns",
    [
        Alcotest.test_case "Patterns 5" `Quick test_patterns_5;
        Alcotest.test_case "Patterns 4" `Quick test_patterns_4;
    ]


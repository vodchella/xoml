open Common
open Engine
open Patterns
open Xoml_test_support

(* Patterns *)

let check_pattern (g: game) (move_str: string) (ptrn: pattern) =
    let p        = point_of_move_str g move_str       in
    let lst, res = pattern_at_point g p X ptrn        in
    let cnt      = pattern_required_points_count ptrn in
    Alcotest.(check bool) ("pattern " ^ (string_of_pattern_values ptrn.kind ptrn.dir) ^ " found") true res;
    Alcotest.(check int)  ("pattern " ^ (string_of_pattern_values ptrn.kind ptrn.dir) ^ " occupied points count") cnt (List.length lst)


let test_patterns_50 () =
    let g = init_test_board ()  in
    let g = apply_move g X "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    check_pattern g "B5" (pattern_find PAT50 E);

    let g = init_test_board ()  in
    let g = apply_move g X "B5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "E2" in
    let g = apply_move g X "F1" in
    check_pattern g "B5" (pattern_find PAT50 SE);

    let g = init_test_board ()  in
    let g = apply_move g X "B5" in
    let g = apply_move g X "B4" in
    let g = apply_move g X "B3" in
    let g = apply_move g X "B2" in
    let g = apply_move g X "B1" in
    check_pattern g "B5" (pattern_find PAT50 S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    let g = apply_move g X "B1" in
    check_pattern g "F5" (pattern_find PAT50 SW)

let test_patterns_42 () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    check_pattern g "C5" (pattern_find PAT42 E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    let g = apply_move g X "F2" in
    check_pattern g "C5" (pattern_find PAT42 SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    let g = apply_move g X "C2" in
    check_pattern g "C5" (pattern_find PAT42 S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    check_pattern g "F5" (pattern_find PAT42 SW)

let test_patterns_41L () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    let g = apply_move g O "G5" in
    check_pattern g "C5" (pattern_find PAT41L E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    let g = apply_move g X "F2" in
    let g = apply_move g O "G1" in
    check_pattern g "C5" (pattern_find PAT41L SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    let g = apply_move g X "C2" in
    let g = apply_move g O "C1" in
    check_pattern g "C5" (pattern_find PAT41L S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    let g = apply_move g O "B1" in
    check_pattern g "F5" (pattern_find PAT41L SW)

let test_patterns_41R () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    check_pattern g "C5" (pattern_find PAT41R E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    let g = apply_move g X "F2" in
    check_pattern g "C5" (pattern_find PAT41R SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    let g = apply_move g X "C2" in
    check_pattern g "C5" (pattern_find PAT41R S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    check_pattern g "F5" (pattern_find PAT41R SW)

let test_patterns_41H1 () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    (* let g = apply_move g X "D5" in *)
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "G5" in
    let g = apply_move g O "H5" in
    check_pattern g "C5" (pattern_find PAT41H1 E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    (* let g = apply_move g X "D4" in *)
    let g = apply_move g X "E3" in
    let g = apply_move g X "F2" in
    let g = apply_move g X "G1" in
    let g = apply_move g O "H1" in
    check_pattern g "C5" (pattern_find PAT41H1 SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    (* let g = apply_move g X "C4" in *)
    let g = apply_move g X "C3" in
    let g = apply_move g X "C2" in
    let g = apply_move g X "C1" in
    let g = apply_move g O "C0" in
    check_pattern g "C5" (pattern_find PAT41H1 S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    (* let g = apply_move g X "E4" in *)
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    let g = apply_move g X "B1" in
    let g = apply_move g O "A0" in
    check_pattern g "F5" (pattern_find PAT41H1 SW)

let test_patterns_41H2 () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    (* let g = apply_move g X "E5" in *)
    let g = apply_move g X "F5" in
    let g = apply_move g X "G5" in
    let g = apply_move g O "H5" in
    check_pattern g "C5" (pattern_find PAT41H2 E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    (* let g = apply_move g X "E3" in *)
    let g = apply_move g X "F2" in
    let g = apply_move g X "G1" in
    let g = apply_move g O "H1" in
    check_pattern g "C5" (pattern_find PAT41H2 SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    (* let g = apply_move g X "C3" in *)
    let g = apply_move g X "C2" in
    let g = apply_move g X "C1" in
    let g = apply_move g O "C0" in
    check_pattern g "C5" (pattern_find PAT41H2 S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    (* let g = apply_move g X "D3" in *)
    let g = apply_move g X "C2" in
    let g = apply_move g X "B1" in
    let g = apply_move g O "A0" in
    check_pattern g "F5" (pattern_find PAT41H2 SW)

let test_patterns_41H3 () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    (* let g = apply_move g X "F5" in *)
    let g = apply_move g X "G5" in
    let g = apply_move g O "H5" in
    check_pattern g "C5" (pattern_find PAT41H3 E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    (* let g = apply_move g X "F2" in *)
    let g = apply_move g X "G1" in
    let g = apply_move g O "H1" in
    check_pattern g "C5" (pattern_find PAT41H3 SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    (* let g = apply_move g X "C2" in *)
    let g = apply_move g X "C1" in
    let g = apply_move g O "C0" in
    check_pattern g "C5" (pattern_find PAT41H3 S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    (* let g = apply_move g X "C2" in *)
    let g = apply_move g X "B1" in
    let g = apply_move g O "A0" in
    check_pattern g "F5" (pattern_find PAT41H3 SW)

let test_patterns_32 () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    check_pattern g "C5" (pattern_find PAT32 E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    check_pattern g "C5" (pattern_find PAT32 SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    check_pattern g "C5" (pattern_find PAT32 S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    check_pattern g "F5" (pattern_find PAT32 SW)

let test_patterns_31L () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    let g = apply_move g O "G5" in
    check_pattern g "C5" (pattern_find PAT31L E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    let g = apply_move g O "G1" in
    check_pattern g "C5" (pattern_find PAT31L SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    let g = apply_move g O "C1" in
    check_pattern g "C5" (pattern_find PAT31L S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    let g = apply_move g O "B1" in
    check_pattern g "F5" (pattern_find PAT31L SW)

let test_patterns_31R () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g X "E5" in
    check_pattern g "C5" (pattern_find PAT31R E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g X "E3" in
    check_pattern g "C5" (pattern_find PAT31R SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g X "C3" in
    check_pattern g "C5" (pattern_find PAT31R S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g X "D3" in
    check_pattern g "F5" (pattern_find PAT31R SW)

let test_patterns_31H1 () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    (* let g = apply_move g X "D5" in *)
    let g = apply_move g X "E5" in
    let g = apply_move g X "F5" in
    let g = apply_move g O "G5" in
    check_pattern g "C5" (pattern_find PAT31H1 E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    (* let g = apply_move g X "D4" in *)
    let g = apply_move g X "E3" in
    let g = apply_move g X "F2" in
    let g = apply_move g O "G1" in
    check_pattern g "C5" (pattern_find PAT31H1 SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    (* let g = apply_move g X "C4" in *)
    let g = apply_move g X "C3" in
    let g = apply_move g X "C2" in
    let g = apply_move g O "C1" in
    check_pattern g "C5" (pattern_find PAT31H1 S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    (* let g = apply_move g X "E4" in *)
    let g = apply_move g X "D3" in
    let g = apply_move g X "C2" in
    let g = apply_move g O "B1" in
    check_pattern g "F5" (pattern_find PAT31H1 SW)

let test_patterns_31H2 () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    (* let g = apply_move g X "E5" in *)
    let g = apply_move g X "F5" in
    let g = apply_move g O "G5" in
    check_pattern g "C5" (pattern_find PAT31H2 E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    (* let g = apply_move g X "E3" in *)
    let g = apply_move g X "F2" in
    let g = apply_move g O "G1" in
    check_pattern g "C5" (pattern_find PAT31H2 SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    (* let g = apply_move g X "C3" in *)
    let g = apply_move g X "C2" in
    let g = apply_move g O "C1" in
    check_pattern g "C5" (pattern_find PAT31H2 S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    (* let g = apply_move g X "D3" in *)
    let g = apply_move g X "C2" in
    let g = apply_move g O "B1" in
    check_pattern g "F5" (pattern_find PAT31H2 SW)

let test_patterns_22 () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    check_pattern g "C5" (pattern_find PAT22 E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    check_pattern g "C5" (pattern_find PAT22 SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    check_pattern g "C5" (pattern_find PAT22 S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    check_pattern g "F5" (pattern_find PAT22 SW)

let test_patterns_21L () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    let g = apply_move g O "E5" in
    check_pattern g "C5" (pattern_find PAT21L E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    let g = apply_move g O "E3" in
    check_pattern g "C5" (pattern_find PAT21L SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    let g = apply_move g O "C3" in
    check_pattern g "C5" (pattern_find PAT21L S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    let g = apply_move g O "D3" in
    check_pattern g "F5" (pattern_find PAT21L SW)

let test_patterns_21R () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D5" in
    check_pattern g "C5" (pattern_find PAT21R E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "D4" in
    check_pattern g "C5" (pattern_find PAT21R SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    let g = apply_move g X "C4" in
    check_pattern g "C5" (pattern_find PAT21R S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    let g = apply_move g X "E4" in
    check_pattern g "F5" (pattern_find PAT21R SW)

let test_patterns_12 () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    check_pattern g "C5" (pattern_find PAT12 E);
    check_pattern g "C5" (pattern_find PAT12 SE);
    check_pattern g "C5" (pattern_find PAT12 S);
    check_pattern g "C5" (pattern_find PAT12 SW)

let test_patterns_11L () =
    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g O "D5" in
    check_pattern g "C5" (pattern_find PAT11L E);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g O "D4" in
    check_pattern g "C5" (pattern_find PAT11L SE);

    let g = init_test_board ()  in
    let g = apply_move g X "C5" in
    let g = apply_move g O "C4" in
    check_pattern g "C5" (pattern_find PAT11L S);

    let g = init_test_board ()  in
    let g = apply_move g X "F5" in
    let g = apply_move g O "E4" in
    check_pattern g "F5" (pattern_find PAT11L SW)

let test_patterns_11R () =
    let g = init_test_board ()  in
    let g = apply_move g O "B5" in
    let g = apply_move g X "C5" in
    check_pattern g "C5" (pattern_find PAT11R E);

    let g = init_test_board ()  in
    let g = apply_move g O "B6" in
    let g = apply_move g X "C5" in
    check_pattern g "C5" (pattern_find PAT11R SE);

    let g = init_test_board ()  in
    let g = apply_move g O "C6" in
    let g = apply_move g X "C5" in
    check_pattern g "C5" (pattern_find PAT11R S);

    let g = init_test_board ()  in
    let g = apply_move g O "G6" in
    let g = apply_move g X "F5" in
    check_pattern g "F5" (pattern_find PAT11R SW)


let suite : string * unit Alcotest.test_case list =
    "Patterns",
    [
        Alcotest.test_case "Patterns 50"   `Quick test_patterns_50;
        Alcotest.test_case "Patterns 42"   `Quick test_patterns_42;
        Alcotest.test_case "Patterns 41L"  `Quick test_patterns_41L;
        Alcotest.test_case "Patterns 41R"  `Quick test_patterns_41R;
        Alcotest.test_case "Patterns 41H1" `Quick test_patterns_41H1;
        Alcotest.test_case "Patterns 41H2" `Quick test_patterns_41H2;
        Alcotest.test_case "Patterns 41H3" `Quick test_patterns_41H3;
        Alcotest.test_case "Patterns 32"   `Quick test_patterns_32;
        Alcotest.test_case "Patterns 31L"  `Quick test_patterns_31L;
        Alcotest.test_case "Patterns 31R"  `Quick test_patterns_31R;
        Alcotest.test_case "Patterns 31H1" `Quick test_patterns_31H1;
        Alcotest.test_case "Patterns 31H2" `Quick test_patterns_31H2;
        Alcotest.test_case "Patterns 22"   `Quick test_patterns_22;
        Alcotest.test_case "Patterns 21L"  `Quick test_patterns_21L;
        Alcotest.test_case "Patterns 21R"  `Quick test_patterns_21R;
        Alcotest.test_case "Patterns 12"   `Quick test_patterns_12;
        Alcotest.test_case "Patterns 11L"  `Quick test_patterns_11L;
        Alcotest.test_case "Patterns 11R"  `Quick test_patterns_11R;
    ]


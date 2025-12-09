open Common
open Engine
open Xoml_test_support

(* Directions and points *)

let test_direction_between_points () =
    let g  = init_test_board () in

    (* F4 <-> I1 : SE / NW *)
    let p1 = Common.point_of_move_str g "F4" in
    let p2 = Common.point_of_move_str g "I1" in
    let d1 = get_direction_between_points p1 p2 in
    let d2 = get_direction_between_points p2 p1 in
    Alcotest.(check direction_testable) "F4 -> I1 dir must be SE" SE (d1 |> Option.get);
    Alcotest.(check direction_testable) "I1 -> F4 dir must be NW" NW (d2 |> Option.get);

    (* F1 <-> I4 : NE / SW *)
    let p1 = Common.point_of_move_str g "F1" in
    let p2 = Common.point_of_move_str g "I4" in
    let d1 = get_direction_between_points p1 p2 in
    let d2 = get_direction_between_points p2 p1 in
    Alcotest.(check direction_testable) "F1 -> I4 dir must be NE" NE (d1 |> Option.get);
    Alcotest.(check direction_testable) "I4 -> F1 dir must be SW" SW (d2 |> Option.get);

    (* F1 <-> I1 : E / W *)
    let p1 = Common.point_of_move_str g "F1" in
    let p2 = Common.point_of_move_str g "I1" in
    let d1 = get_direction_between_points p1 p2 in
    let d2 = get_direction_between_points p2 p1 in
    Alcotest.(check direction_testable) "F1 -> I1 dir must be E" E (d1 |> Option.get);
    Alcotest.(check direction_testable) "I1 -> F1 dir must be W" W (d2 |> Option.get);

    (* F1 <-> F4 : N / S *)
    let p1 = Common.point_of_move_str g "F1" in
    let p2 = Common.point_of_move_str g "F4" in
    let d1 = get_direction_between_points p1 p2 in
    let d2 = get_direction_between_points p2 p1 in
    Alcotest.(check direction_testable) "F1 -> F4 dir must be N" N (d1 |> Option.get);
    Alcotest.(check direction_testable) "F4 -> F1 dir must be S" S (d2 |> Option.get);

    (* F1 -> F1 : None *)
    let p1 = Common.point_of_move_str g "F1" in
    let p2 = Common.point_of_move_str g "F1" in
    let d  = get_direction_between_points p1 p2 in
    Alcotest.(check (option direction_testable)) "F1 -> F1 dir must be None" None d

let test_line_points () =
    let g  = init_test_board () in
    let p1 = Common.point_of_move_str g "D7" in
    let p2 = Common.point_of_move_str g "E6" in
    let p3 = Common.point_of_move_str g "F5" in
    let p4 = Common.point_of_move_str g "G4" in
    let points = get_line_points p1 p4 in
    Alcotest.(check int) "points count between D7 and G4 must be 4" 4 (List.length points);
    match points with
    | [tp1; tp2; tp3; tp4] ->
        Alcotest.(check point_testable) "first  point must be D7" p1 tp1;
        Alcotest.(check point_testable) "second point must be E6" p2 tp2;
        Alcotest.(check point_testable) "third  point must be F5" p3 tp3;
        Alcotest.(check point_testable) "fourth point must be G4" p4 tp4
    | _ ->
        Alcotest.fail "Expected exactly 4 points"

let suite : string * unit Alcotest.test_case list =
    "Directions and points",
    [
        Alcotest.test_case "Directions between different points" `Quick test_direction_between_points;
        Alcotest.test_case "Line points"                         `Quick test_line_points;
    ]


let init_test_board () =
    let board_side = 10 in
    let g = Common.initial_game in
    let g = { g with
              board_width   = board_side
            ; board_height  = board_side
            ; board_size    = board_side * board_side
            ; board = Array.make (board_side * board_side) None
            ; win_length    = 5
            }
    in
    g

let point_testable =
    let pp fmt { Common.x; y } =
        Format.fprintf fmt "{ x = %d; y = %d }" x y
    in
    Alcotest.testable pp ( = )


let test_no_open_ends_1 () =
    let g = init_test_board () in
    let g = Engine.apply_move g X "B8" in
    let g = Engine.apply_move g O "C7" in
    let g = Engine.apply_move g X "D6" in
    let score, p1, p2 = Engine.score_line g O (Common.point_of_move_str g "C7") SE in
    Alcotest.(check int) "score must be zero" 0 score;
    Alcotest.(check point_testable) "points must be equal" p1 p2

let test_no_open_ends_2 () =
    let g = init_test_board () in
    let g = Engine.apply_move g O "A9" in
    let g = Engine.apply_move g O "B8" in
    let g = Engine.apply_move g O "C7" in
    let g = Engine.apply_move g X "D6" in
    let score, p1, p2 = Engine.score_line g O (Common.point_of_move_str g "C7") SE in
    let tp1 = Common.point_of_move_str g "C7" in
    let tp2 = Common.point_of_move_str g "A9" in
    Alcotest.(check int) "score must be zero" 0 score;
    Alcotest.(check point_testable) "first  point must be C7" p1 tp1;
    Alcotest.(check point_testable) "second point must be A9" p2 tp2

let test_one_open_end () =
    let g = init_test_board () in
    let g = Engine.apply_move g O "H2" in
    let g = Engine.apply_move g O "I1" in
    let g = Engine.apply_move g O "J0" in
    let score, p1, p2 = Engine.score_line g O (Common.point_of_move_str g "I1") NW in
    let tp1 = Common.point_of_move_str g "H2" in
    let tp2 = Common.point_of_move_str g "J0" in
    Alcotest.(check int) "score must be 500" 500 score;
    Alcotest.(check point_testable) "first  point must be H2" p1 tp1;
    Alcotest.(check point_testable) "second point must be J0" p2 tp2

let test_two_open_ends () =
    let g = init_test_board () in
    let g = Engine.apply_move g O "F4" in
    let g = Engine.apply_move g O "G3" in
    let g = Engine.apply_move g O "H2" in
    let g = Engine.apply_move g O "I1" in
    let score, p1, p2 = Engine.score_line g O (Common.point_of_move_str g "G3") NW in
    let tp1 = Common.point_of_move_str g "F4" in
    let tp2 = Common.point_of_move_str g "I1" in
    Alcotest.(check int) "score must be 100000" 100000 score;
    Alcotest.(check point_testable) "first  point must be F4" p1 tp1;
    Alcotest.(check point_testable) "second point must be I1" p2 tp2

let () =
    Alcotest.run "all_tests" [
        ( "Score line"
        , [
            Alcotest.test_case "No open ends,  line length: 1" `Quick test_no_open_ends_1;
            Alcotest.test_case "No open ends,  line length: 3" `Quick test_no_open_ends_2;
            Alcotest.test_case "One open end,  line length: 3" `Quick test_one_open_end;
            Alcotest.test_case "Two open ends, line length: 4" `Quick test_two_open_ends;
          ]
        );
    ]

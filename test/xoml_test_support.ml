open Common

let init_test_board () =
    let board_side = 10 in
    let g = initial_game in
    let g = { g with
              board_width   = board_side
            ; board_height  = board_side
            ; board_size    = board_side * board_side
            ; board = Array.make (board_side * board_side) None
            ; win_length    = 5
            }
    in
    g

let move_str_of_point (p : Common.point) : string =
    let x_to_letter x = Char.chr (Char.code 'A' + x - 1) in
    let y_to_digit  y = Char.chr (Char.code '0' + (10 - y)) in
    let c1 = x_to_letter p.x in
    let c2 = y_to_digit  p.y in
    String.make 1 c1 ^ String.make 1 c2

let point_testable =
    let pp fmt p =
        Format.fprintf fmt "%s { x = %d; y = %d }"
            (move_str_of_point p)
            p.x
            p.y
    in
    Alcotest.testable pp ( = )

let direction_testable =
    let pp fmt d =
        Format.fprintf fmt "%s" (Common.string_of_direction d)
    in
    Alcotest.testable pp ( = )


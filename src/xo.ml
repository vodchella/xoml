open Common


let calc_next_move (_g: game) (_p: player) =
    Unix.sleep 2;
    "RND"

let rec main_loop (g: game) =
    Board.draw g;
    Board.print_prompt g;
    match g.state with
    | Thinking ->
        let next_move = calc_next_move g O in
        let g' = { g with
                   last_tip = "Computer's move: '" ^ next_move ^ "'. Now it's your turn..."
                 ; last_move_str = next_move
                 ; state = Waiting
                 }
        in
        (main_loop[@tailcall]) g'
    | Waiting ->
        let input =
            read_line ()
            |> String.trim
            |> String.uppercase_ascii
            |> Input.of_string g
        in
        match input with
        | Invalid invalid_input ->
            let g' = { g with last_tip = invalid_input } in
            (main_loop[@tailcall]) g'
        | Move move ->
            let g' = Board.apply_move g X move in
            (main_loop[@tailcall]) g'
        | Help ->
            let g' = { g with last_tip = inital_tip } in
            (main_loop[@tailcall]) g'
        | Quit -> ()

let main () =
    let s = Board.size_from_args () in
    let g = initial_game in
    let g = { g with board_width = s; board_height = s; input_vmargin = s + 5 } in
    assert (g.board_width  >= 5);
    assert (g.board_width  <= 10);
    assert (g.board_height >= 5);
    assert (g.board_height <= 10);
    screen_clear ();
    (main_loop[@tailcall]) g
let () = main ()


open Common


let rec main_loop (g: game) =
    Board.print_last_figure g;
    let winner = Engine.find_winner g in
    match winner with
    | Some winner' ->
        Board.print_congratulations g winner';
        ()
    | None ->
        Board.print_prompt g;
        match g.state with
        | Thinking ->
            let next_move = Engine.calc_next_move g O in
            let next_move_str = next_move |> Option.value ~default:"RND" in
            let g' = { g with
                       last_tip = "Computer's move: '" ^ next_move_str ^ "'. Now it's your turn..."
                     ; last_move_str   = None
                     ; last_move_point = None
                     ; last_move_index = None
                     ; last_player = Some O
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
    Printexc.record_backtrace true;
    let s = Board.size_from_args () in
    let g = initial_game in
    let g = { g with
              board_width = s
            ; board_height = s
            ; board_size = s * s
            ; input_vmargin = s + 5
            }
    in
    assert (g.board_width  >= 5);
    assert (g.board_width  <= 10);
    assert (g.board_height >= 5);
    assert (g.board_height <= 10);
    assert (g.win_length   >= 3);
    assert (g.win_length   <= 5);
    screen_clear ();
    Board.draw g;
    (main_loop[@tailcall]) g
let () = main ()


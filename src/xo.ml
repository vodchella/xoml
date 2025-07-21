open Common


let rec main_loop (g: game) =
    Board.print_last_figure g;
    match Engine.find_winner g with
    | Some winner ->
        Board.print_congratulations g winner;
        ()
    | None ->
        Board.print_prompt g;
        match g.state with
        | Thinking ->
            let next_move_index = Engine.find_best_move g O in
            let g' = Board.apply_move_by_index g O next_move_index in
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
    Printexc.record_backtrace true;
    Random.self_init ();
    Board.screen_clear ();
    Board.draw g;
    (main_loop[@tailcall]) g
let () = main ()


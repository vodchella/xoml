open Common


let calc_next_move (_g: game) (_p: player) =
    Unix.sleep 2;
    None

let check_winner_on_last_move (g: game) =
    match g.last_move_point with
    | None -> NP
    | Some point  ->
        let finished_lines_count = all_directions
            |> List.map (fun dir -> count_in_direction g g.last_player point dir)  (* TODO: break on success *)
            |> List.filter (fun cnt -> cnt == g.win_length)
            |> List.length
        in
        match finished_lines_count with
        | 0 -> NP
        | _ -> g.last_player

let rec main_loop (g: game) =
    Board.print_last_figure g;
    let winner = check_winner_on_last_move g in
    match winner with
    | X | O ->
            Board.print_congratulations g winner;
            ()
    | NP ->
        Board.print_prompt g;
        match g.state with
        | Thinking ->
            let next_move = calc_next_move g O in
            let next_move_str = next_move |> Option.value ~default:"RND" in
            let g' = { g with
                       last_tip = "Computer's move: '" ^ next_move_str ^ "'. Now it's your turn..."
                     ; last_move_str   = None
                     ; last_move_point = None
                     ; last_move_index = None
                     ; last_player = O
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
    assert (g.win_length   >= 3);
    assert (g.win_length   <= 5);
    screen_clear ();
    Board.draw g;
    (main_loop[@tailcall]) g
let () = main ()


open Common


let rec main_loop (g: game) =
    (* Board.print_last_figure g; *)
    Board.print_all_figures g;
    match Engine.find_winner g with
    | Some winner ->
        Board.print_congratulations g winner;
        ()
    | None ->
        Board.print_prompt g;
        match g.state with
        | Thinking -> (
            match Engine.find_best_move g O with
            | Some i ->
                let g' = Engine.apply_move_by_index g O i in
                Logger.write_game_move_by_index g O i;
                (main_loop[@tailcall]) g'
            | _ ->
                Board.print_draw g )
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
                let g' = Engine.apply_move g X move in
                Logger.write_game_move g X move;
                (main_loop[@tailcall]) g'
            | Help ->
                let g' = { g with last_tip = initial_tip } in
                (main_loop[@tailcall]) g'
            | Quit -> ()


let rec main_gtp_loop (g: game) =
    let input =
        read_line ()
        |> String.trim
        |> String.uppercase_ascii
        |> Input.of_gtp_string g
    in
    match input with
    | Quit -> (
        print_endline "= QUIT\n";
    )
    | Name -> (
        print_endline "= XOml\n";
        (main_gtp_loop[@tailcall]) g
    )
    | Version -> (
        print_endline "= 0.3.0\n";
        (main_gtp_loop[@tailcall]) g
    )
    | ProtocolVersion -> (
        print_endline "= 1\n";
        (main_gtp_loop[@tailcall]) g
    )
    | Play (pl, move_str) -> (
        let g' = Engine.apply_move g pl move_str in
        if g'.last_move_ok then (
            print_endline "= PLAY\n";
            Logger.write_game_move g pl move_str;
        )
        else print_endline "? cell is occupied \n";
        (main_gtp_loop[@tailcall]) g'
    )
    | GenMove pl -> (
        match Engine.find_best_move g pl with
        | Some i ->
            let g' = Engine.apply_move_by_index g pl i in
            print_endline ("= " ^ (move_str_of_index g i) ^ "\n");
            Logger.write_game_move_by_index g pl i;
            (main_gtp_loop[@tailcall]) g'
        | _ -> (
            print_endline "? draw\n";
            (main_gtp_loop[@tailcall]) g
        )
    )
    | Winner -> (
        ( (* Parens are important!!! *)
            match Engine.find_winner g with
            | Some winner -> print_endline ("= " ^ (string_of_player winner) ^ "\n")
            | None -> (
                if Engine.is_draw g
                then print_endline "= draw\n"
                else print_endline "= ?\n"
            )
        );
        (main_gtp_loop[@tailcall]) g
    )
    | CleanBoard -> (
        let g = init_board_with_side g g.board_width in
        let g = Logger.reset g in
        print_endline "= CLEAN_BOARD\n";
        (main_gtp_loop[@tailcall]) g
    )
    | ShowBoard -> (
        print_endline "= SHOW_BOARD";
        Board.print g;
        print_endline "\n";
        (main_gtp_loop[@tailcall]) g
    )
    | BoardSize board_side -> (
        let g = init_board_with_side g board_side in
        print_endline ("= BOARD_SIZE\n");
        (main_gtp_loop[@tailcall]) g
    )
    | Unknown err -> (
        print_endline ("? " ^ err ^ "\n");
        (main_gtp_loop[@tailcall]) g
    )



let main () =
    let { board_side; playerO_starts; gtp_mode } = Args.parse_args in
    let g = initial_game in
    let g = init_board_with_side g board_side in
    assert (g.board_width  >= 5);
    assert (g.board_width  <= 10);
    assert (g.board_height >= 5);
    assert (g.board_height <= 10);
    assert (g.win_length   >= 3);
    assert (g.win_length   <= 5);
    let g = Logger.create g in

    Printexc.record_backtrace true;
    Random.self_init ();

    if gtp_mode then
        (main_gtp_loop[@tailcall]) g
    else (
        let g = if playerO_starts then (
            let first_move = Engine.find_best_move g O in
            let g = Engine.apply_move_by_index_opt g O first_move in
            g
        ) else g in
        Board.screen_clear ();
        Board.draw g;
        (main_loop[@tailcall]) g
    )

let () = main ()


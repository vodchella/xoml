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
        | Thinking -> (
            Unix.sleep 1;
            match Engine.find_best_move g O with
            | Some i ->
                let g' = Engine.apply_move_by_index g O i in
                (main_loop[@tailcall]) g'
            | _ ->
                Board.print_draw g)
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
                (main_loop[@tailcall]) g'
            | Help ->
                let g' = { g with last_tip = initial_tip } in
                (main_loop[@tailcall]) g'
            | Quit -> ()

let main () =
    let { board_side; playerO_starts } = Args.parse_args in
    let g = initial_game in
    (* let g = Logger.create g in *)
    let g = { g with
              board_width   = board_side
            ; board_height  = board_side
            ; board_size    = board_side * board_side
            ; input_vmargin = board_side + 5
            ; board = Array.make (board_side * board_side) None
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

    (* Board.print_all_figures g; *)
    (* Benchmark.bench_game_fn ~count:1000000 g "score_board" Engine.score_board_test; *)

    let first_move = some_if playerO_starts (Engine.find_best_move g O) in
    let g = Engine.apply_move_by_index_opt g O first_move in
    Board.screen_clear ();
    Board.draw g;
    (main_loop[@tailcall]) g

let () = main ()


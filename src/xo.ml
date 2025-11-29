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
    let s = Board.size_from_args () in
    let g = initial_game in
    let g = Logger.create g in
    let g = { g with
              board_width = s
            ; board_height = s
            ; board_size = s * s
            ; input_vmargin = s + 5
            ; board = Array.make (s * s) None
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

    (* Benchmark.bench_game_fn g "get_possible_moves" Engine.get_possible_moves; *)
    (* Benchmark.bench_game_fn g "get_occupied_indices" Engine.get_occupied_indices; *)

    Board.screen_clear ();
    Board.draw g;
    (main_loop[@tailcall]) g

let () = main ()


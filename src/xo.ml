open Common


let rec main_loop (g: game) =
    Board.print_last_figure g;
    (* Board.print_all_figures g; *)
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
        print_endline ("=\n");
    )
    | Name -> (
        print_endline ("= XOML engine\n");
        (main_gtp_loop[@tailcall]) g
    )
    | Version -> (
        print_endline ("= 0.1.9\n");
        (main_gtp_loop[@tailcall]) g
    )
    | ProtocolVersion -> (
        print_endline ("= 1\n");
        (main_gtp_loop[@tailcall]) g
    )
    | Play (pl, move_str) -> (
        let g' = Engine.apply_move g pl move_str in
        if g'.last_move_ok then (
            print_endline("= \n");
        )
        else print_endline("? cell is occupied \n");
        (main_gtp_loop[@tailcall]) g'
    )
    | GenMove pl -> (
        match Engine.find_best_move g pl with
        | Some i ->
            let g' = Engine.apply_move_by_index g pl i in
            print_endline ("= " ^ (move_str_of_index g i) ^ "\n");
            (main_gtp_loop[@tailcall]) g'
        | _ ->
            print_endline "? draw\n"
    )
    | Winner -> (
        print_endline ("? not implemented\n");
        (main_gtp_loop[@tailcall]) g
    )
    | CleanBoard -> (
        print_endline ("? not implemented\n");
        (main_gtp_loop[@tailcall]) g
    )
    | ShowBoard -> (
        print_endline ("? not implemented\n");
        (main_gtp_loop[@tailcall]) g
    )
    | BoardSize size -> (
        print_endline ("? not implemented " ^ (string_of_int size) ^ "\n");
        (main_gtp_loop[@tailcall]) g
    )
    | Unknown err -> (
        print_endline ("? " ^ err ^ "\n");
        (main_gtp_loop[@tailcall]) g
    )



let main () =
    let { board_side; playerO_starts; gtp_mode } = Args.parse_args in
    let g = initial_game in
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

    if gtp_mode then
        (main_gtp_loop[@tailcall]) g
    else (
        let first_move = some_if playerO_starts (Engine.find_best_move g O) in
        let g = Logger.create g in
        let g = Engine.apply_move_by_index_opt g O first_move in
        Board.screen_clear ();
        Board.draw g;

        (* Board.print_all_figures g; *)
        (* Board.print_prompt g; *)
        (* failwith "Stop"; *)

        (main_loop[@tailcall]) g
    )

let () = main ()


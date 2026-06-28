open Common


let parse_args : run_args =
    let playerO_starts = ref false in
    let gtp_mode       = ref false in
    let size           = ref None  in
    let difficulty     = ref 0     in

    let speclist = [
      ("-s", Arg.Int (fun x ->
          if x < 5 || x > 10 then
            raise (Arg.Bad "Value for -s must be between 5 and 10")
          else
            size := Some x),
       "Set board size (must be between 5 and 10)");

      ("-d", Arg.Int (fun x ->
          if x < 0 || x > 1 then
            raise (Arg.Bad "Value for -d must be 0 or 1")
          else
            difficulty := x),
       "Set game difficulty (must be must be 0 (easy) or 1 (normal))");

      ("-O", Arg.Set playerO_starts,
       "Player O (computer) starts first");

      ("-GTP", Arg.Set gtp_mode,
       "Launch in GTP-mode");

      ("--version", Arg.Unit (fun () ->
           print_endline ("XOml " ^ engine_version);
           exit 0),
       "Show version information");
    ] in

    let usage_msg = "Usage: program_name [-s X] [-d X] [-O] [-GTP] [--version]" in
    Arg.parse speclist print_endline usage_msg;

    let board_side = Option.value !size ~default:default_board_width in
    let difficulty = if !difficulty = 0 then Easy else Normal        in
    { board_side
    ; playerO_starts  = !playerO_starts
    ; gtp_mode        = !gtp_mode
    ; difficulty
    }

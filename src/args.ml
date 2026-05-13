open Common


let parse_args : run_args =
    let playerO_starts = ref false in
    let gtp_mode = ref false in
    let size = ref None in

    let speclist = [
      ("-s", Arg.Int (fun x ->
          if x < 5 || x > 10 then
            raise (Arg.Bad "Value for -s must be between 5 and 10")
          else
            size := Some x),
       "Set size (must be between 5 and 10)");

      ("-O", Arg.Set playerO_starts,
       "Player O (computer) starts first");

      ("-GTP", Arg.Set gtp_mode,
       "Launch in GTP-mode");
    ] in

    let usage_msg = "Usage: program_name [-s X] [-O] [-GTP]" in
    Arg.parse speclist print_endline usage_msg;

    let board_side = Option.value !size ~default:default_board_width in
    { board_side
    ; playerO_starts = !playerO_starts
    ; gtp_mode       = !gtp_mode
    }

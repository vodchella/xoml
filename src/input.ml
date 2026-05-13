open Common


type input_result =
    | Move of string
    | Invalid of string
    | Help
    | Quit

type gtp_input_result =
    | Unknown of string
    | Name
    | Version
    | ProtocolVersion
    | Play of player * string
    | GenMove of player
    | Winner
    | CleanBoard
    | ShowBoard
    | BoardSize of int
    | Quit

let str_is_valid_move (g: game) str =
    match String.length str with
    | 2 ->
        let c1 = String.get str 0 in
        let c2 = String.get str 1 in
        let is_letter c =
            let code = chrc c in
            code >= chrc 'A' && code <= chrc 'A' + g.board_width - 1
        in
        let is_digit c =
            let code = chrc c in
            code >= chrc '0' && code <= chrc '0' + g.board_height - 1
        in
        is_letter c1 && is_digit c2
    | _ -> false

let of_string (g: game) (s: string) : input_result =
    match s with
    | "Q" | "E" | "X" | "QUIT" | "EXIT" -> Quit
    | "H" | "HELP"                      -> Help
    | s when str_is_valid_move g s      -> Move s
    | s                                 -> Invalid ("Invalid move: '" ^ s ^ "'")

let split_gtp_string_and_validate s =
    let parts = s
        |> String.split_on_char ' '
        |> List.map String.trim
        |> List.filter (fun x -> x <> "")
    in
    let len = List.length parts in
    if len > 3 || len < 2 then
        Error ("invalid arguments count: " ^ s)
    else
        Ok parts

let of_gtp_string (g: game) (s: string) : gtp_input_result =
    match s with
    | "QUIT" | "Q"       -> Quit
    | "NAME"             -> Name
    | "VERSION"          -> Version
    | "PROTOCOL_VERSION" -> ProtocolVersion
    | "CLEAN_BOARD"      -> CleanBoard
    | "SHOW_BOARD"       -> ShowBoard
    | "WINNER"           -> Winner
    | str                -> (
        match split_gtp_string_and_validate str with
        | Ok parts  -> (
            let arg_len = List.length parts - 1 in
            let cmd_str = List.nth parts 0 in
            match cmd_str with
            | "PLAY" -> (
                if arg_len == 2 then (
                    let player_str = List.nth parts 1 in
                    match player_of_string player_str with
                    | Some pl -> (
                        let move_str   = List.nth parts 2 in
                        if str_is_valid_move g move_str then
                            Play (pl, move_str)
                        else
                            Unknown ("unknown move: " ^ move_str)
                    )
                    | None -> Unknown ("unknown player: " ^ player_str)
                )
                else Unknown ("invalid arguments count: " ^ cmd_str)
            )
            | "GEN_MOVE" -> (
                if arg_len == 1 then (
                    let player_str = List.nth parts 1 in
                    match player_of_string player_str with
                    | Some pl -> GenMove pl
                    | None    -> Unknown ("unknown player: " ^ player_str)
                )
                else Unknown ("invalid arguments count: " ^ cmd_str)
            )
            | "BOARD_SIZE" -> (
                if arg_len == 1 then (
                    let size_opt = List.nth parts 1 |> int_of_string_opt in
                    match size_opt with
                    | Some size -> (
                        if size >= 5 && size <= 10 then (
                            BoardSize size
                        )
                        else Unknown ("invalid board size")
                    )
                    | None -> Unknown ("invalid board size")
                )
                else Unknown ("invalid arguments count: " ^ cmd_str)
            )
            | _         -> Unknown ("unknown command: " ^ s)
        )
        | Error msg -> Unknown msg
    )

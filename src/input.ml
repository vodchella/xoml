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

let of_gtp_string (_g: game) (s: string) : gtp_input_result =
    match s with
    | "QUIT"             -> Quit
    | "NAME"             -> Name
    | "VERSION"          -> Version
    | "PROTOCOL_VERSION" -> ProtocolVersion
    | _                  -> Unknown ("unknown command: " ^ s ^ "\n")

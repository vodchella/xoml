open Common

type input_result =
    | Move of string
    | Invalid of string
    | Help
    | Quit

let input_str_is_valid_move (g: game) str =
    match String.length str with
    | 2 ->
        let c1 = String.get str 0 in
        let c2 = String.get str 1 in
        let is_letter c =
            let code = chrc c in
            let boundU = chrc 'A' + g.board_width - 1 in
            let boundL = chrc 'a' + g.board_width - 1 in
                (code >= chrc 'A' && code <= boundU) ||
                (code >= chrc 'a' && code <= boundL)
        in
        let is_digit c =
            let code = chrc c in
                code >= chrc '0' && code <= chrc '0' + g.board_height - 1
        in
        is_letter c1 && is_digit c2
    | _ -> false

let of_string (g: game) = function
    | "q" | "e" | "quit" | "exit" -> Quit
    | "h" | "help" -> Help
    | s when (input_str_is_valid_move g s) -> Move s
    | s -> Invalid ("Invalid move: " ^ s)

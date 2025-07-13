open Common

type input_result =
    | Move of string
    | Invalid of string
    | Help
    | Quit

let input_str_is_valid_move str =
    match String.length str with
    | 2 ->
        let c1 = String.get str 0 in
        let c2 = String.get str 1 in
        let is_letter c =
            let code = chrc c in
                (code >= chrc 'A' && code <= chrc 'Z') ||
                (code >= chrc 'a' && code <= chrc 'z')
        in
        let is_digit c =
            let code = chrc c in
                code >= chrc '0' && code <= chrc '9'
        in
        is_letter c1 && is_digit c2
    | _ -> false

let of_string = function
    | "q" | "e" | "quit" | "exit" -> Quit
    | "h" | "help" -> Help
    | s when (input_str_is_valid_move s) -> Move s
    | s -> Invalid ("Invalid move: " ^ s)

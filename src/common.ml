let printf = Printf.printf
let chrc   = Char.code


type game =
    { last_tip : string
    ; board    : int list
    }

let board_width   = 10
let board_height  = 10
let input_vmargin = board_height + 5
let input_prompt  = "?> "
let ascii_esc     = "\x1b"
let inital_tip    = "Type your move (e.g. 'E4') or 'Q' to exit"
let empty_row     = String.make (String.length inital_tip) ' '
let initial_game  =
    { last_tip = inital_tip
    ; board    = []
    }


let screen_clear () =
    Sys.command "clear"
    |> ignore

let cursor_move row col =
    printf "%s[%d;%dH%!" ascii_esc row col
    |> ignore

let print_at str row col =
    cursor_move row col;
    printf "%s" str
    |> ignore

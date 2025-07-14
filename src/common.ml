let printf = Printf.printf
let chrc   = Char.code


type game_state = Waiting | Thinking
type game =
    { board_width   : int
    ; board_height  : int
    ; input_vmargin : int
    ; last_tip      : string
    ; last_move_str : string
    ; state         : game_state
    ; board         : int list
    }

let default_board_width  = 10
let default_board_height = 10
let input_prompt         = "?> "
let ascii_esc            = "\x1b"
let inital_tip           = "Type your move (e.g. 'E4') or 'Q' to exit"
let empty_row            = String.make (String.length inital_tip) ' '
let initial_game         =
    { board_width   = default_board_width
    ; board_height  = default_board_height
    ; input_vmargin = default_board_height + 5
    ; last_tip      = inital_tip
    ; last_move_str = ""
    ; state         = Waiting
    ; board         = []
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

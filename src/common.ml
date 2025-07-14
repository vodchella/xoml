let printf = Printf.printf
let chrc   = Char.code


type player = X | O | N
type game_state = Waiting | Thinking
type point = { x : int
             ; y : int
             }
type game =
    { board_width   : int
    ; board_height  : int
    ; input_vmargin : int
    ; last_tip      : string
    ; last_move_str : string
    ; state         : game_state
    ; board         : player array
    }

let default_board_width  = 10
let default_board_height = 10
let input_prompt         = "?> "
let ascii_esc            = "\x1b"
let sym_x                = "X"
let sym_o                = "O"
let sym_none             = "."
let inital_tip           = "Type your move (e.g. 'E4') or 'Q' to exit"
let empty_row            = String.make (5 + (String.length inital_tip)) ' '
let initial_game         =
    { board_width   = default_board_width
    ; board_height  = default_board_height
    ; input_vmargin = 0  (* Will be set up on startup *)
    ; last_tip      = inital_tip
    ; last_move_str = ""
    ; state         = Waiting
    ; board         = Array.make (default_board_width * default_board_height) N
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

let index_of_point (g: game) (p : point) : int =
    (p.y - 1) * g.board_width + (p.x - 1)

let point_of_index (g : game) (i : int) : point =
  let x = (i mod g.board_width) + 1 in
  let y = (i / g.board_width) + 1 in
  { x; y }

let point_of_move_str (g: game) (s : string) : point =
    let letter_to_x c =
        chrc c - chrc 'A' + 1 in
    let digit_to_int c =
        chrc c - chrc '0' in
    let c1 = s.[0] in
    let c2 = s.[1] in
    let x  = letter_to_x c1 in
    let y' = digit_to_int c2 in
    let y  = g.board_height - y' in
    { x; y }

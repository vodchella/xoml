let printf = Printf.printf
let chrc   = Char.code


type direction  = N | E | S | W | NE | SE | SW | NW
type player     = X | O
type game_state = Waiting | Thinking
type point = { x : int
             ; y : int
             }
type game =
    { board_width     : int
    ; board_height    : int
    ; board_size      : int
    ; board           : player option array
    ; win_length      : int
    ; input_vmargin   : int
    ; last_tip        : string
    ; last_move_str   : string option
    ; last_move_point : point option
    ; last_move_index : int option
    ; last_player     : player option
    ; state           : game_state
    }

let default_board_width  = 10
let default_board_height = 10
let default_win_length   = 5
let input_prompt         = "?> "
let ascii_esc            = "\x1b"
let sym_x                = "X"
let sym_o                = "O"
let sym_none             = "."
let inital_tip           = "Type your move (e.g. 'E4') or 'Q' to exit"
let empty_row            = String.make (5 + (String.length inital_tip)) ' '
let initial_game         =
    { board_width     = default_board_width
    ; board_height    = default_board_height
    ; board_size      = default_board_width * default_board_height  (* Will be set up on startup *)
    ; board           = Array.make (default_board_width * default_board_height) None
    ; win_length      = default_win_length
    ; input_vmargin   = default_board_height + 5                    (* Will be set up on startup *)
    ; last_tip        = inital_tip
    ; last_move_str   = None
    ; last_move_point = None
    ; last_move_index = None
    ; last_player     = None
    ; state           = Waiting
    }


let ( >>! ) opt fn =
    match opt with
    | Some v -> Some v
    | None   -> fn ()

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

let print_symbol_at_point symbol point =
    print_at symbol (point.y + 2) (point.x * 2 + 2)

let index_of_point (g: game) (pnt : point) : int option =
    match pnt with
    | { x; _ } when x < 1 -> None
    | { y; _ } when y < 1 -> None
    | { x; _ } when x > g.board_width  -> None
    | { y; _ } when y > g.board_height -> None
    | { x; y } ->
        Some ((y - 1) * g.board_width + (x - 1))

let point_of_index (g : game) (index : int) : point option =
    match index with
    | i when i > (g.board_size - 1) -> None
    | i when i < 0 -> None
    | i ->
        let x = (i mod g.board_width) + 1 in
        let y = (i  /  g.board_width) + 1 in
        Some { x; y }

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

let move_str_of_point (g : game) (p : point) : string =
    let x_to_letter x = Char.chr (Char.code 'A' + x - 1) in
    let y_to_digit y = Char.chr (Char.code '0' + (g.board_height - y)) in
    let c1 = x_to_letter p.x in
    let c2 = y_to_digit p.y in
    String.make 1 c1 ^ String.make 1 c2


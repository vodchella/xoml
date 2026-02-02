let printf = Printf.printf
let chrc   = Char.code


type direction  = N | E | S | W | NE | SE | SW | NW
type player     = X | O
type game_state = Waiting | Thinking

(* INFO: point is one-based !!! *)
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
    ; log_file        : out_channel option
    ; log_moves       : bool
    }
type run_args =
    { board_side      : int
    ; playerO_starts  : bool
    }

let score_win            = 10_000_000
let score_insta_win      = score_win * 4
let score_fork           = score_win / 2
let score_4_2            = score_win
let score_4_1            = 900
let score_3_2            = 1000
let score_3_1            = 100
let score_2_2            = 50
let score_2_1            = 10
let score_1_2            = 2
let score_1_1            = 1

let default_board_width  = 10
let default_board_height = 10
let default_win_length   = 5
let input_prompt         = "?> "
let ascii_esc            = "\x1b"
let sym_x                = "X"
let sym_o                = "O"
let sym_none             = "."
let initial_tip          = "Type your move (e.g. 'E4') or 'Q' to exit"
let empty_row            = String.make (5 + (String.length initial_tip)) ' '
let initial_game         =
    { board_width     = default_board_width
    ; board_height    = default_board_height
    ; board_size      = default_board_width * default_board_height  (* Will be set up on startup *)
    ; board           = Array.make (default_board_width * default_board_height) None
    ; win_length      = default_win_length
    ; input_vmargin   = default_board_height + 5                    (* Will be set up on startup *)
    ; last_tip        = initial_tip
    ; last_move_str   = None
    ; last_move_point = None
    ; last_move_index = None
    ; last_player     = None
    ; state           = Waiting
    ; log_file        = None
    ; log_moves       = true
    }


let ( >>! ) opt fn =
    match opt with
    | Some v -> Some v
    | None   -> fn ()

let some_if cond value = if cond then value else None

let string_of_player = function
    | X -> sym_x
    | O -> sym_o

let index_of_point (g: game) (pnt: point) : int option =
    match pnt with
    | { x; _ } when x < 1 -> None
    | { y; _ } when y < 1 -> None
    | { x; _ } when x > g.board_width  -> None
    | { y; _ } when y > g.board_height -> None
    | { x; y } ->
        Some ((y - 1) * g.board_width + (x - 1))

let point_of_index (g: game) (index : int) : point option =
    match index with
    | i when i > (g.board_size - 1) -> None
    | i when i < 0                  -> None
    | i ->
        let x = (i mod g.board_width) + 1 in
        let y = (i  /  g.board_width) + 1 in
        Some { x; y }

let point_of_move_str (g: game) (s: string) : point =
    let letter_to_x  c = chrc c - chrc 'A' + 1 in
    let digit_to_int c = chrc c - chrc '0'     in
    let c1 = s.[0] in
    let c2 = s.[1] in
    let x  = letter_to_x  c1 in
    let y' = digit_to_int c2 in
    let y  = g.board_height - y' in
    { x; y }

let index_of_move_str (g: game) (s: string) : int =
    s
    |> point_of_move_str g
    |> index_of_point g
    |> Option.get

let move_str_of_point (g : game) (p: point) : string =
    let x_to_letter x = Char.chr (Char.code 'A' + x - 1) in
    let y_to_digit  y = Char.chr (Char.code '0' + (g.board_height - y)) in
    let c1 = x_to_letter p.x in
    let c2 = y_to_digit  p.y in
    String.make 1 c1 ^ String.make 1 c2

let move_str_of_index (g: game) (index: int) : string =
    index
    |> point_of_index g
    |> Option.get
    |> move_str_of_point g

let string_of_direction = function
    | N  -> "N"
    | E  -> "E"
    | S  -> "S"
    | W  -> "W"
    | NE -> "NE"
    | SE -> "SE"
    | SW -> "SW"
    | NW -> "NW"

(* NOTE: just for debugging *)
let string_of_int_direction_list (lst : (int * direction) list) : string =
    let items =
        List.map (fun (i, dir) ->
            Printf.sprintf "(%d, %s)" i (string_of_direction dir)
        ) lst
    in
    "[" ^ String.concat "; " items ^ "]"

let get_board_center_borders = function
    (* returns: (offset, side) *)
    | 10 -> (3, 4)
    |  9 -> (3, 3)
    |  8 -> (2, 4)
    |  7 -> (2, 3)
    |  6 -> (2, 2)
    |  5 -> (2, 1)
    |  _ -> failwith "Invalid board size"

let random_index_near_center_opt (g: game) =
    let (offset, side) = get_board_center_borders g.board_width in
    let x = offset + (1 + Random.int side) in
    let y = offset + (1 + Random.int side) in
    index_of_point g {x; y}

let random_index_near_center (g: game) =
    random_index_near_center_opt g |> Option.get


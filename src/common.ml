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
    ; last_move_ok    : bool
    ; last_player     : player option
    ; state           : game_state
    ; log_file        : out_channel option
    ; log_moves       : bool
    }
type run_args =
    { board_side      : int
    ; playerO_starts  : bool
    ; gtp_mode        : bool
    }


let working_dirs         = [ SW; S; SE; E ]

let score_inevitable_win = 10_000
let score_insta_win      = score_inevitable_win * 5
let score_fork           = score_inevitable_win / 2
let score_4_2            = score_inevitable_win
let score_4_1            = 90
let score_3_2            = 100
let score_3_1            = 10
let score_2_2            = 5
let score_2_1            = 1
let score_1_2            = 0
let score_1_1            = 0

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
    ; last_move_ok    = false
    ; last_player     = None
    ; state           = Waiting
    ; log_file        = None
    ; log_moves       = true
    }


let fst3 (x, _, _) = x

let pairs_of_triples (triples: ('a * 'b * 'c) list) : ('a * 'b) list =
    triples
    |> List.map (fun (a, b, _) -> (a, b))

let ( >>! ) opt fn =
    match opt with
    | Some v -> Some v
    | None   -> fn ()

let opposite_direction_of = function
    | N  ->  S
    | E  ->  W
    | S  ->  N
    | W  ->  E
    | NE ->  SW
    | SE ->  NW
    | SW ->  NE
    | NW ->  SE

let opponent_of = function
    | X -> O
    | O -> X

let string_of_player = function
    | X -> sym_x
    | O -> sym_o

let player_of_string = function
    | "X" -> Some X
    | "O" -> Some O
    | _     -> None

let point_is_valid (g: game) (pnt: point) : bool =
    match pnt with
    | { x; _ } when x < 1 -> false
    | { y; _ } when y < 1 -> false
    | { x; _ } when x > g.board_width  -> false
    | { y; _ } when y > g.board_height -> false
    | _ -> true

let index_of_point (g: game) (pnt: point) : int option =
    if point_is_valid g pnt then
        Some ((pnt.y - 1) * g.board_width + (pnt.x - 1))
    else None

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

let get_direction_between_points (p1 : point) (p2 : point) : direction option =
    let { x = x1; y = y1 } = p1 in
    let { x = x2; y = y2 } = p2 in
    let cx = compare x2 x1 in
    let cy = compare y2 y1 in
    match cx, cy with
    |  0,  0  -> None
    | -1,  0  -> Some W
    |  1,  0  -> Some E
    |  0, -1  -> Some N
    |  0,  1  -> Some S
    | -1, -1  -> Some NW
    | -1,  1  -> Some SW
    |  1, -1  -> Some NE
    |  1,  1  -> Some SE
    | _       -> None

let string_of_direction = function
    | N  -> "N"
    | E  -> "E"
    | S  -> "S"
    | W  -> "W"
    | NE -> "NE"
    | SE -> "SE"
    | SW -> "SW"
    | NW -> "NW"

let relative_point_of_direction = function
    | N  -> { x =  0; y = -1 }
    | E  -> { x =  1; y =  0 }
    | S  -> { x =  0; y =  1 }
    | W  -> { x = -1; y =  0 }
    | NE -> { x =  1; y = -1 }
    | SE -> { x =  1; y =  1 }
    | SW -> { x = -1; y =  1 }
    | NW -> { x = -1; y = -1 }

let init_board_with_side (g: game) (board_side : int) : game =
    { g with
      board_width   = board_side
    ; board_height  = board_side
    ; board_size    = board_side * board_side
    ; input_vmargin = board_side + 5
    ; board = Array.make (board_side * board_side) None
    }

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

let filter_board_indices (g : game) (fn : int * player option -> bool) : int list =
    let acc = ref [] in
    g.board |>
    Array.iteri (fun i cell ->
        if fn (i, cell) then acc := i :: !acc
    );
    !acc

let get_occupied_indices (g: game) (pl: player) : int list =
    let pl_opt = Some pl in
    filter_board_indices g (fun (_, v) -> v = pl_opt)

let shift_point_according_to_direction (p: point) (d: direction) (times: int) : point =
    match times with
    | 0 -> p
    | _ ->
        let rel = relative_point_of_direction d in
        { x = p.x + (rel.x * times)
        ; y = p.y + (rel.y * times)
        }

let count_in_direction (g: game) (pl: player) (p: point) (d: direction) : int =
    let rec count_in_direction' counter =
        match counter with
        | c when c > g.win_length -> g.win_length
        | c ->
            let point = shift_point_according_to_direction p d c in
            match index_of_point g point with
            | None       -> c
            | Some index ->
                match g.board.(index) with
                | Some p' when p' == pl -> count_in_direction' (c + 1)
                | _ -> c
    in
    count_in_direction' 0

let points_intersection_size points1 points2 =
    List.fold_left
        (fun acc p -> if List.mem p points2 then acc + 1 else acc)
        0 points1


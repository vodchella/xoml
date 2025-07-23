open Common


let working_dirs = [ SW; S; SE; E ]

let relative_point_of_direction = function
    | N  -> { x =  0; y = -1 }
    | E  -> { x =  1; y =  0 }
    | S  -> { x =  0; y =  1 }
    | W  -> { x = -1; y =  0 }
    | NE -> { x =  1; y = -1 }
    | SE -> { x =  1; y =  1 }
    | SW -> { x = -1; y =  1 }
    | NW -> { x = -1; y = -1 }

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

let shift_point_according_to_direction (p: point) (d: direction) (times: int) : point =
    let rel = relative_point_of_direction d in
    let point =
        { x = p.x + (rel.x * times)
        ; y = p.y + (rel.y * times)
        }
    in
    point

let count_in_direction (g: game) (pl: player) (p: point) (d: direction) : int =
    let rec count_in_direction_aux counter =
        match counter with
        | c when c > g.win_length -> g.win_length
        | c ->
            let point = shift_point_according_to_direction p d c in
            match index_of_point g point with
            | None       -> c
            | Some index ->
                match g.board.(index) with
                | Some p' when p' == pl -> count_in_direction_aux (c + 1)
                | _ -> c
    in
    count_in_direction_aux 0

let have_open_end_in_direction (g: game) (p: point) (d: direction) (cnt_in_dir: int) : int =
    let point = shift_point_according_to_direction p d cnt_in_dir in
    match index_of_point g point with
    | None       -> 0
    | Some index ->
        match g.board.(index) with
        | None -> 1
        | _    -> 0

let get_possible_moves (g: game) : int list =
    g.board
    |> Array.to_list
    |> List.mapi   (fun  i  v  -> (i, v))
    |> List.filter (fun (_, v) -> v == None)
    |> List.map    fst

let apply_move_by_index (g: game) (p: player) index : game =
    let point = point_of_index g index |> Option.get in
    let move_str = move_str_of_point g point in
    let cell = g.board.(index) in
    let new_state = match p with
        | O -> Waiting
        | X -> Thinking
    in
    let new_tip = match p with
        | X -> "Your move: '" ^ move_str ^ "'. Thinking..."
        | O -> "Computer's move: '" ^ move_str ^ "'. Now it's your turn..."
    in
    match cell with
    | None ->
        g.board.(index) <- Some p;
        let g' = { g with
                   last_tip        = new_tip
                 ; last_move_str   = Some move_str
                 ; last_move_point = Some point
                 ; last_move_index = Some index
                 ; last_player     = Some p
                 ; state           = new_state
                 }
        in
        g'
    | Some _ ->
        let g' = { g with last_tip = "Cell '" ^ move_str ^ "' is occupied" } in
        g'

let apply_move (g: game) (pl: player) move_str : game =
    let point = point_of_move_str g move_str in
    let index = index_of_point g point |> Option.get in
    (* let score = evaluate_position g pl index in *)
    (* printf "score = %d" score; *)
    apply_move_by_index g pl index

let find_winner (g: game) : player option =
    let check_player_at_index index pl =
        let point = point_of_index g index |> Option.get in
        let rec check_player_at_index_aux dirs =
            match dirs with
            | []          -> None
            | dir :: rest ->
                match count_in_direction g pl point dir with
                | c when c == g.win_length -> Some pl
                | _ -> check_player_at_index_aux rest
        in
        check_player_at_index_aux working_dirs
    in
    let rec find_winner_aux index =
        match index with
        | i when i > (g.board_size - 1) -> None
        | _ ->
            check_player_at_index index X
            >>! (fun () -> check_player_at_index index O)
            >>! (fun () -> find_winner_aux       (index + 1))
    in
    find_winner_aux 0

let score_line (g: game) (pl: player) (p: point) (dir: direction) : int =
    let index = index_of_point g p|> Option.get in
    match g.board.(index) with
    | None ->
        let opp_dir         = opposite_direction_of dir in
        let shifted_point_1 = shift_point_according_to_direction p dir     1  in
        let shifted_point_2 = shift_point_according_to_direction p opp_dir 1  in
        let cnt_in_dir_1    = count_in_direction g pl shifted_point_1 dir     in
        let cnt_in_dir_2    = count_in_direction g pl shifted_point_2 opp_dir in
        let opened_1        = have_open_end_in_direction g shifted_point_1 dir     cnt_in_dir_1 in
        let opened_2        = have_open_end_in_direction g shifted_point_2 opp_dir cnt_in_dir_2 in
        let count           = cnt_in_dir_1 + cnt_in_dir_2 in
        let open_ends       = opened_1 + opened_2 in
        let score = match count, open_ends with
            | count', _ when count' >= 4 -> 10000
            | 3, 2 -> 1000
            | 2, 2 -> 100
            | 1, 2 -> 10
            | _ -> 0
        in
        score
    | _ -> failwith "Cell must be empty"

let evaluate_position (g: game) (pl: player) (index: int) : int =
    let point = point_of_index g index |> Option.get in
    let pl_opp = opponent_of pl in
    let rec evaluate_position_aux dirs accum =
        match dirs with
        | [] -> accum
        | d :: rest ->
            let score_pl = score_line g pl     point d in
            let score_op = score_line g pl_opp point d in
            let score    = (score_pl + score_op) in
            evaluate_position_aux rest (score + accum)
    in
    evaluate_position_aux working_dirs 0

let evaluate_board (g: game) (pl: player) : int =
    let initial_score = -1 lsl (Sys.int_size - 1) in
    let rec evaluate_board_aux index best_score_accum best_index_accum =
        match index with
        | i when i > (g.board_size - 1) -> best_index_accum
        | i ->
            let score = evaluate_position g pl i in
            match score with
            | score' when score' > best_score_accum -> evaluate_board_aux (i + 1) score' i
            | _ -> evaluate_board_aux (i + 1) best_score_accum best_index_accum
    in
    evaluate_board_aux 0 initial_score (-1)

let find_best_move_random (possible_moves: int list) : int =
    let len   = List.length possible_moves      in
    let rndi  = Random.int  len                 in
    let index = List.nth    possible_moves rndi in
    index

let find_best_move (g: game) (_p: player) : int =
    match get_possible_moves g with
    | []    -> failwith "OOOPS: board is full!"
    | moves ->
        let index = find_best_move_random moves in
        Unix.sleep 1;
        index


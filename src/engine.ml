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

let score_of = function
    | (c, _) when c >= 5 -> 1000000
    | (4, 2) -> 100000
    | (4, 1) -> 10000
    | (3, 2) -> 1000
    | (3, 1) -> 500
    | (2, 2) -> 50
    | (2, 1) -> 10
    | (1, 2) -> 2
    | (1, 1) -> 1
    | _      -> 0

let shift_point_according_to_direction (p: point) (d: direction) (times: int) : point =
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

let have_open_end_in_direction (g: game) (p: point) (d: direction) (cnt_in_dir: int) : int =
    let point = shift_point_according_to_direction p d cnt_in_dir in
    match index_of_point g point with
    | None       -> 0
    | Some index ->
        match g.board.(index) with
        | None -> 1
        | _    -> 0

let filter_board_indices (g : game) (fn: int * 'a option -> bool) : int list =
    let acc = ref [] in
    let b = g.board in
    for i = Array.length b - 1 downto 0 do
        if fn (i, b.(i)) then
            acc := i :: !acc
    done;
    !acc

let get_occupied_indices (g: game) : int list =
    filter_board_indices g (fun (_, v) -> v <> None)

let get_active_bounds_rect (g: game) : point * point * int =
    let cnt = ref 0 in
    let p1  = ref {x = max_int; y = max_int} in
    let p2  = ref {x = min_int; y = min_int} in
    for i = 0 to g.board_size - 1 do
        let pl = g.board.(i) in
        match pl with
        | Some _ ->
            (* PERF: This can be optimized to avoid
                     rewriting points that haven’t changed *)
            let p   = point_of_index g i |> Option.get   in
            let nx1 = if p.x < !p1.x then p.x else !p1.x in
            let ny1 = if p.y < !p1.y then p.y else !p1.y in
            let nx2 = if p.x > !p2.x then p.x else !p2.x in
            let ny2 = if p.y > !p2.y then p.y else !p2.y in
            cnt := !cnt + 1;
            p1  := {x = nx1; y = ny1};
            p2  := {x = nx2; y = ny2};
        | None -> ()
    done;
    ( !p1, !p2, !cnt )

let expand_bounds (g: game) (p1: point) (p2: point) (factor: int) : (point * point) =
    let nx1 = p1.x - factor in
    let ny1 = p1.y - factor in
    let nx2 = p2.x + factor in
    let ny2 = p2.y + factor in
    let nx1 = if nx1 < 1 then 1 else nx1 in
    let ny1 = if ny1 < 1 then 1 else ny1 in
    let nx2 = if nx2 > g.board_width  then g.board_width  else nx2 in
    let ny2 = if ny2 > g.board_height then g.board_height else ny2 in
    ( {x = nx1; y = ny1}, {x = nx2; y = ny2} )

let get_possible_moves (g: game) : int list =
    let (p1, p2, cnt) = get_active_bounds_rect g in
    match cnt with
    | 0 -> (random_index_biased_toward_center g.board_size) :: []
    | _ ->
        let (p1, p2) = expand_bounds g p1 p2 2 in
        let result = ref [] in
        for x = p1.x to p2.x do
            for y = p1.y to p2.y do
                let i = index_of_point g {x; y} |> Option.get in
                let pl = g.board.(i) in
                match pl with
                | Some _ -> ()
                | None -> result := i :: !result
            done;
        done;
        !result

let apply_move_by_index (g: game) (pl: player) index : game =
    let point     = point_of_index g index |> Option.get in
    let move_str  = move_str_of_point g point in
    let cell      = g.board.(index) in
    let new_state = match pl with
        | O -> Waiting
        | X -> Thinking
    in
    let new_tip = match pl with
        | X -> "Your move: '"       ^ move_str ^ "'. Thinking..."
        | O -> "Computer's move: '" ^ move_str ^ "'. Now it's your turn..."
    in
    match cell with
    | None ->
        g.board.(index) <- Some pl;
        { g with
          last_tip        = new_tip
        ; last_move_str   = Some move_str
        ; last_move_point = Some point
        ; last_move_index = Some index
        ; last_player     = Some pl
        ; state           = new_state
        }
    | Some _ ->
        { g with last_tip = "Cell '" ^ move_str ^ "' is occupied" }

let apply_move (g: game) (pl: player) move_str : game =
    let point = point_of_move_str g move_str in
    let index = index_of_point    g point |> Option.get in
    apply_move_by_index g pl index

let find_winner (g: game) : player option =
    let check_player_at_index index pl =
        let pl' = g.board.(index) in
        match pl' with
        | Some pl'' when pl'' = pl ->
            let point = point_of_index g index |> Option.get in
            let rec check_player_at_index' dirs =
                match dirs with
                | []          -> None
                | dir :: rest ->
                    match count_in_direction g pl point dir with
                    | c when c == g.win_length -> Some pl
                    | _ -> check_player_at_index' rest
            in
            check_player_at_index' working_dirs
        | _ -> None
    in
    (* PERF: need to be improved with get_active_bounds_rect usage *)
    let rec find_winner' index =
        match index with
        | i when i > (g.board_size - 1) -> None
        | _ ->
            check_player_at_index index X
            >>! (fun () -> check_player_at_index index O)
            >>! (fun () -> find_winner'          (index + 1))
    in
    find_winner' 0

let score_line (g: game) (pl: player) (p: point) (dir: direction) : int =
    let index           = index_of_point g p |> Option.get in
    let opp_dir         = opposite_direction_of dir in
    let shifted_point_1 = shift_point_according_to_direction p dir     1  in
    let shifted_point_2 = shift_point_according_to_direction p opp_dir 1  in
    let cnt_in_dir_1    = count_in_direction g pl shifted_point_1 dir     in
    let cnt_in_dir_2    = count_in_direction g pl shifted_point_2 opp_dir in
    let opened_1        = have_open_end_in_direction g shifted_point_1 dir     cnt_in_dir_1 in
    let opened_2        = have_open_end_in_direction g shifted_point_2 opp_dir cnt_in_dir_2 in
    let count           = cnt_in_dir_1 + cnt_in_dir_2 in
    let open_ends       = opened_1 + opened_2 in
    match g.board.(index) with
    | Some pl' when pl' = pl -> score_of ((count + 1), open_ends)
    | None                   -> score_of ( count     , open_ends)
    | _                      -> 0

let score_position (g: game) (pl: player) (index: int) : int =
    let point = point_of_index g index |> Option.get in
    let pl_opp = opponent_of pl in
    let rec score_position' dirs accum =
        match dirs with
        | [] -> accum
        | d :: rest ->
            let score_pl = score_line g pl     point d in
            let score_op = score_line g pl_opp point d in
            let score    = (score_pl + (score_op / 2)) in
            score_position' rest (score + accum)
    in
    score_position' working_dirs 0

let score_board (g: game) (pl: player) : int =
    let indices = get_occupied_indices g in
    let rec score_board' indexes accum =
        match indexes with
        | [] -> accum
        | i :: rest ->
            let score_at_index = score_position g pl i in
            let pl' = g.board.(i) |> Option.get in
            let v = match pl' with
                    | p' when p' == pl' -> 1
                    | _ -> 0
            in
            score_board' rest (accum + (v * score_at_index))
    in
    score_board' indices 0

let find_best_move_score (g: game) (pl: player) (m: int list) : int option =
    match m with
    | []    -> None
    | moves ->
        let rec find_best_move_score' moves score_accum index_accum =
            match moves with
            | []        -> score_accum, index_accum
            | i :: rest ->
               match score_position g pl i with
               | score when score > score_accum ->
                   find_best_move_score' rest score (Some i)
               | _ ->
                   find_best_move_score' rest score_accum index_accum
        in
        let score, index = find_best_move_score' moves min_int None in
        match score with
        |  0 ->
            moves
            |> List.length
            |> random_index_biased_toward_center
            |> List.nth_opt moves
        | _ -> index

let find_best_move (g: game) (pl: player) : int option =
    let moves = get_possible_moves g in
    find_best_move_score g pl moves


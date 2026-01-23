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
    | (c, _) when c >= 5 -> win_score
    | (4, 2) -> win_score
    | (4, 1) -> 900
    | (3, 2) -> 1000
    | (3, 1) -> 100
    | (2, 2) -> 50
    | (2, 1) -> 10
    | (1, 2) -> 2
    | (1, 1) -> 1
    | _      -> 0

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

let get_line_points (p1: point) (p2: point) : point list =
    match get_direction_between_points p1 p2 with
    | None ->
        [p1]
    | Some dir ->
        let step = relative_point_of_direction dir in
        let rec build acc p =
            if p = p2 then
                List.rev (p :: acc)
            else
                let next = { x = p.x + step.x; y = p.y + step.y } in
                build (p :: acc) next
        in
        build [] p1

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

let have_open_end_in_direction (g: game) (p: point) (d: direction) (cnt_in_dir: int) : int =
    let point = shift_point_according_to_direction p d cnt_in_dir in
    match index_of_point g point with
    | None       -> 0
    | Some index ->
        match g.board.(index) with
        | None -> 1
        | _    -> 0

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

let get_active_bounds_rect (g: game) : point * point * int =
    let cnt = ref 0 in
    let p1  = ref { x = max_int; y = max_int } in
    let p2  = ref { x = min_int; y = min_int } in
    g.board |>
    Array.iteri (fun i cell ->
        match cell with
        | Some _ ->
            (* PERF: This can be optimized to avoid
                     rewriting points that haven’t changed *)
            let p   = Option.get (point_of_index g i) in
            let nx1 = min p.x (!p1).x in
            let ny1 = min p.y (!p1).y in
            let nx2 = max p.x (!p2).x in
            let ny2 = max p.y (!p2).y in
            cnt := !cnt + 1;
            p1  := { x = nx1; y = ny1 };
            p2  := { x = nx2; y = ny2 };
        | None -> ()
    );
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
    (* TODO: If the player places a piece near the wall, the computer should take the center *)
    (* Now computer makes its first move next to the piece placed by the player *)
    let expand_factor = if cnt = 1 then 1 else 2 in
    match cnt with
    | 0 -> (random_index_near_center g) :: []
    | _ ->
        let (p1, p2) = expand_bounds g p1 p2 expand_factor in
        let result = ref [] in
        for x = p1.x to p2.x do
            for y = p1.y to p2.y do
                let i  = index_of_point g {x; y} |> Option.get in
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

let apply_move_by_index_opt (g: game) (pl: player) (index_opt: int option) : game =
    match index_opt with
    | Some index -> apply_move_by_index g pl index
    | None       -> g

let apply_move (g: game) (pl: player) move_str : game =
    let point = point_of_move_str g move_str in
    let index = index_of_point    g point |> Option.get in
    apply_move_by_index g pl index

(* TODO: optimize like score_board, so that visited cells are not checked again *)
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
    (* TODO: need to be improved with get_active_bounds_rect usage *)
    let rec find_winner' index =
        match index with
        | i when i > (g.board_size - 1) -> None
        | _ ->
            check_player_at_index index X
            >>! (fun () -> check_player_at_index index O)
            >>! (fun () -> find_winner'          (index + 1))
    in
    find_winner' 0

let score_line (g: game) (pl: player) (p: point) (dir: direction) : int * point * point =
    let index = index_of_point g p |> Option.get in
    match g.board.(index) with
    | None -> ( 0, p, p )
    | _ ->
        let opp_dir         = opposite_direction_of dir in
        let shifted_point_1 = shift_point_according_to_direction p dir     1  in
        let shifted_point_2 = shift_point_according_to_direction p opp_dir 1  in
        let cnt_in_dir_1    = count_in_direction g pl shifted_point_1 dir     in
        let cnt_in_dir_2    = count_in_direction g pl shifted_point_2 opp_dir in
        let opened_1        = have_open_end_in_direction g shifted_point_1 dir     cnt_in_dir_1 in
        let opened_2        = have_open_end_in_direction g shifted_point_2 opp_dir cnt_in_dir_2 in
        let count           = cnt_in_dir_1 + cnt_in_dir_2 + 1 in
        let open_ends       = opened_1 + opened_2 in
        let edge_point_1    = shift_point_according_to_direction p dir     cnt_in_dir_1 in
        let edge_point_2    = shift_point_according_to_direction p opp_dir cnt_in_dir_2 in
        ( score_of (count, open_ends), edge_point_1, edge_point_2 )

let score_position (g: game) (pl: player) (index: int) (dirs: direction list) : int * ((int * direction) list) =
    let point = point_of_index g index |> Option.get in
    let rec score_position' dirs score_accum indicies_accum =
        match dirs with
        | [] -> (score_accum, indicies_accum)
        | d :: rest ->
            let score, p1, p2 = score_line g pl point d in
            let points = get_line_points p1 p2 in
            let processed_indicies = points |> List.map (fun p ->
                let i = index_of_point g p |> Option.get in
                (i, d)
            )
            in
            score_position' rest (score + score_accum) (indicies_accum @ processed_indicies)
    in
    score_position' dirs 0 []

let init_dirs_array (g: game) (indicies: int list) : direction list option array =
    let dir_arr = Array.make g.board_size None in
    indicies
    |> List.iter (fun i ->
        dir_arr.(i) <- Some working_dirs
    );
    dir_arr

let remove_dir_opt (d: direction) (lst: direction list) : direction list option =
    let rec aux acc = function
        | [] ->
            let result = List.rev acc in
            if result = [] then None else Some result
        | x :: xs when x = d ->
            let result = List.rev_append acc xs in
            if result = [] then None else Some result
        | x :: xs ->
            aux (x :: acc) xs
    in
    aux [] lst

let score_board (g: game) (pl: player) : int =
    let indicies = get_occupied_indices g pl  in
    let dir_arr  = ref (init_dirs_array g indicies) in
    let actualize_dir_array indicies_and_dirs_to_remove =
        indicies_and_dirs_to_remove
        |> List.iter (fun ind_dir ->
            let ind, dir = ind_dir in
            match !dir_arr.(ind) with
            | Some arr ->
                let new_arr = remove_dir_opt dir arr in
                !dir_arr.(ind) <- new_arr
            | None -> ()
        )
    in
    let rec score_board' indexes accum =
        match indexes with
        | [] -> accum
        | i :: rest ->
            match !dir_arr.(i) with
            | None ->
                score_board' rest accum
            | Some dirs ->
                let score, processed_indicies = score_position g pl i dirs in
                (* NOTE: use string_of_int_direction_list to view processed_indicies: *)
                (*       print_endline ((string_of_int i) ^ "i: " ^
                                       (Common.string_of_int_direction_list processed_indicies)); *)
                actualize_dir_array processed_indicies;
                score_board' rest (accum + score)
    in
    score_board' indicies 0

let eval_position (g : game) (pl : player) : int =
    let my_score  = score_board g pl in
    let opp_score = score_board g (opponent_of pl) in
    let score     = my_score - opp_score in
    score

let find_best_move (g: game) (pl: player) : int option =
    let max_depth      = 4 in
    let break_on_index = ref None in

    let rec minimax (g: game) (depth: int) (alpha: int) (beta: int) (cur_pl: player) : int =
        (* match find_winner g with *)
        (* | Some p -> *)
        (*     (* failwith ("!!! " ^ (string_of_int depth)); *) *)
        (*     if p = pl then win_score + depth *)
        (*     else -win_score - depth *)
        (* | None -> *)

        if depth <= 0 then
            eval_position g pl
        else
            let moves = get_possible_moves g in
            if moves = [] then
                eval_position g pl
            else if cur_pl = pl then
                (* The "maximizing" player is making a move *)
                let best = ref min_int in
                let a    = ref alpha   in
                let rec loop = function
                    | [] -> !best
                    | m :: rest ->
                        let old_cell = g.board.(m) in
                        g.board.(m) <- Some cur_pl;

                        let score = minimax g (depth - 1) !a beta (opponent_of cur_pl) in
                        g.board.(m) <- old_cell;

                        if score > !best then best := score;
                        if score > !a    then a    := score;

                        if abs(score) >= win_score || Option.is_some !break_on_index then score
                        else if !a >= beta then !best  (* beta-cutoff *)
                        else loop rest
                in
                loop moves
            else if cur_pl != pl then
                (* The "minimizing" player is making a move (the opponent) *)
                let best = ref max_int in
                let b    = ref beta    in
                let rec loop = function
                    | [] -> !best
                    | m :: rest ->
                        let old_cell = g.board.(m) in
                        g.board.(m) <- Some cur_pl;

                        let break = ref false in
                        if depth = (max_depth - 1) then (
                            let opp_score = score_board g cur_pl in
                            if opp_score >= win_score then break := true
                        );

                        if !break then (
                            g.board.(m) <- old_cell;
                            break_on_index := Some m;
                            0  (* Score doesn't matter in this case *)
                        )
                        else (
                            let score = minimax g (depth - 1) alpha !b (opponent_of cur_pl) in
                            g.board.(m) <- old_cell;

                            if score < !best then best := score;
                            if score < !b    then b    := score;

                            if score >= win_score then (
                                break_on_index := Some m;
                                0  (* Score doesn't matter in this case *)
                            )
                            else if alpha >= !b then !best  (* alpha-cutoff *)
                            else loop rest
                        )
                in
                loop moves;
            else
                failwith "unreachable"
    in

    let moves = get_possible_moves g in
    match moves with
    | [] -> None
    | _  ->
        let best_move  = ref None in
        let best_score = ref min_int in
        let alpha      = ref min_int in

        moves
        |> List.iter (fun m ->
            match !break_on_index with
            | Some i ->
                best_move := Some i;
            | _ -> (
                let old_cell = g.board.(m) in
                g.board.(m) <- Some pl;

                let score = minimax g (max_depth - 1) !alpha max_int (opponent_of pl) in
                g.board.(m) <- old_cell;

                if score > !best_score then begin
                    best_score := score;
                    best_move  := Some m;
                end;

                if score > !alpha then alpha := score;
            )
        );

        !best_move


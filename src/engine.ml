open Common
open Engine_normal


let score_board_fn = score_board_normal

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

(* TODO: factor out last_move_ok *)
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
        ; last_move_ok    = true
        ; state           = new_state
        }
    | Some _ ->
        { g with
          last_tip = "Cell '" ^ move_str ^ "' is occupied"
        ; last_move_ok = false
        }

let apply_move_by_index_opt (g: game) (pl: player) (index_opt: int option) : game =
    match index_opt with
    | Some index -> apply_move_by_index g pl index
    | None       -> g

let apply_move (g: game) (pl: player) move_str : game =
    let point = point_of_move_str g move_str in
    let index = index_of_point    g point |> Option.get in
    apply_move_by_index g pl index

let is_draw (g: game) : bool =
    Array.exists (fun c -> Option.is_none c) g.board
    |> Bool.not

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

let eval_position (g : game) (pl : player) : int =
    let my_score  = score_board_fn g pl in
    let opp_score = score_board_fn g (opponent_of pl) in
    let score     = my_score - opp_score in
    score

let find_best_move (g: game) (pl: player) : int option =
    let max_depth      = 4 in
    let break_on_index = ref None in

    let rec minimax (g: game) (depth: int) (alpha: int) (beta: int) (cur_pl: player) : int =
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

                        if abs(score) >= score_inevitable_win || Option.is_some !break_on_index then score
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

                        let score = minimax g (depth - 1) alpha !b (opponent_of cur_pl) in
                        g.board.(m) <- old_cell;

                        if score < !best then best := score;
                        if score < !b    then b    := score;

                        if score >= score_inevitable_win then score
                        else if alpha >= !b then !best  (* alpha-cutoff *)
                        else loop rest
                in
                loop moves;
            else
                failwith "unreachable: minimax"
    in

    let moves = get_possible_moves g in
    match moves with
    | [] -> None
    | _  ->
        let best_move    = ref None    in
        let best_score   = ref min_int in
        let alpha        = ref min_int in
        let win_found    = ref false   in

        (* Check for insta win *)
        moves
        |> List.iter (fun m ->
            if not !win_found then (
                let old_cell = g.board.(m) in
                g.board.(m) <- Some pl;
                let my_score = score_board_fn g pl in
                if my_score >= score_insta_win then (
                    break_on_index := Some m;
                    win_found := true;
                    (* let move_str = move_str_of_index g m in *)
                    (* let pl_str   = string_of_player pl in *)
                    (* Logger.write g ("!!! INSTA WIN FOUND " ^ move_str ^ " " ^ pl_str); *)
                );
                g.board.(m) <- old_cell;
            )
        );

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


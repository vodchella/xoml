open Common


let relative_point_of_direction = function
    | N  -> { x =  0; y = -1 }
    | E  -> { x =  1; y =  0 }
    | S  -> { x =  0; y =  1 }
    | W  -> { x = -1; y =  0 }
    | NE -> { x =  1; y = -1 }
    | SE -> { x =  1; y =  1 }
    | SW -> { x = -1; y =  1 }
    | NW -> { x = -1; y = -1 }

let count_in_direction (g: game) (pl: player) (p: point) (d: direction): int =
    let rel = relative_point_of_direction d in
    let rec count_in_direction_aux counter accum =
        match counter with
        | c when c > g.win_length -> accum
        | c ->
            let pnt =
                { x = p.x + (rel.x * c)
                ; y = p.y + (rel.y * c)
                }
            in
            let index_opt = index_of_point g pnt in
            match index_opt with
            | None -> accum
            | Some index ->
                match g.board.(index) with
                | p_ when p_ == pl -> count_in_direction_aux (c + 1) (accum + 1)
                | _ -> accum
    in
    count_in_direction_aux 0 0

let check_winner (g: game) =
    match g.last_move_point with
    | None -> NP
    | Some point  ->
        let finished_lines_count = all_directions
            |> List.map (fun dir -> count_in_direction g g.last_player point dir)  (* TODO: break on success *)
            |> List.filter (fun cnt -> cnt == g.win_length)
            |> List.length
        in
        match finished_lines_count with
        | 0 -> NP
        | _ -> g.last_player

let calc_next_move (_g: game) (_p: player) =
    Unix.sleep 2;
    None


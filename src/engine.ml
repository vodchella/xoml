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

let count_in_direction (_g: game) (_pl: player) (_p: point) (_d: direction): int =
    0

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


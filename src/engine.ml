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

let find_winner (g: game) : player =
    let check_player_at_index index pl =
        let point = point_of_index g index |> Option.get in
        let rec check_player_at_index_aux dirs =
            match dirs with
            | [] -> false
            | dir :: rest ->
                match count_in_direction g pl point dir with
                | c when c == g.win_length -> true
                | _ -> check_player_at_index_aux rest
        in
        check_player_at_index_aux [ SW; S; SE; E ]
    in
    let rec find_winner_aux index =
        match index with
        | i when i > (g.board_size - 1) -> NP
        | i ->
            match check_player_at_index i X with
            | true  -> X
            | false ->
                match check_player_at_index i O with
                | true  -> O
                | false -> find_winner_aux (i + 1)
    in
    find_winner_aux 0

let calc_next_move (_g: game) (_p: player) =
    Unix.sleep 2;
    None


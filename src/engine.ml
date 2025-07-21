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
    let rec count_in_direction_aux counter =
        match counter with
        | c when c > g.win_length -> g.win_length
        | c ->
            let point =
                { x = p.x + (rel.x * c)
                ; y = p.y + (rel.y * c)
                }
            in
            match index_of_point g point with
            | None -> c
            | Some index ->
                match g.board.(index) with
                | Some p' when p' == pl -> count_in_direction_aux (c + 1)
                | _ -> c
    in
    count_in_direction_aux 0

let get_possible_moves (g: game) : int list =
    g.board
    |> Array.to_list
    |> List.mapi   (fun i v -> (i, v))
    |> List.filter (fun (_, v) -> v == None)
    |> List.map    fst

let find_winner (g: game) : player option =
    let check_player_at_index index pl =
        let point = point_of_index g index |> Option.get in
        let rec check_player_at_index_aux dirs =
            match dirs with
            | [] -> None
            | dir :: rest ->
                match count_in_direction g pl point dir with
                | c when c == g.win_length -> Some pl
                | _ -> check_player_at_index_aux rest
        in
        check_player_at_index_aux [ SW; S; SE; E ]
    in
    let rec find_winner_aux index =
        match index with
        | i when i > (g.board_size - 1) -> None
        | _ ->
            check_player_at_index index X
            >>! (fun () -> check_player_at_index index O)
            >>! (fun () -> find_winner_aux (index + 1))
    in
    find_winner_aux 0

let find_best_move (g: game) (_p: player) =
    let possible_moves = get_possible_moves g in
    match possible_moves with
    | [] -> failwith "OOOPS: board is full!"
    | moves ->
        let len   = List.length moves in
        let rndi  = Random.int len in
        let index = List.nth moves rndi in
        Unix.sleep 1;
        index


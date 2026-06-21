open Common


let score_of = function
    | (c, _) when c >= 5 -> score_insta_win
    | (4, 2) -> score_inevitable_win * 2
    | (4, 1) -> score_4_1
    | (3, 2) -> score_3_2
    | (3, 1) -> score_3_1
    | (2, 2) -> score_2_2
    | (2, 1) -> score_2_1
    | (1, 2) -> score_1_2
    | (1, 1) -> score_1_1
    | _      -> 0

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

let have_open_end_in_direction (g: game) (p: point) (d: direction) (cnt_in_dir: int) : int =
    let point = shift_point_according_to_direction p d cnt_in_dir in
    match index_of_point g point with
    | None       -> 0
    | Some index ->
        match g.board.(index) with
        | None -> 1
        | _    -> 0

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

(* ..OOO. or .OOO.. *)
let check_line_for_fork_pattern_1 (g: game) (pl: player) (p: point) (dir: direction) : int =
    let shift1, shift2 =
        match dir with
        | E | SE | S | SW -> 2, 4
        | W | NW | N | NE -> 4, 2
    in
    let opp = opponent_of pl in
    let check_point (pnt: point) : int =
        match index_of_point g pnt with
        | Some i -> (
            match g.board.(i) with
            | Some player when player = opp -> 0
            | _ -> 1 )
        | None   -> 0
    in
    let p1 = shift_point_according_to_direction p dir shift1 in
    let p2 = shift_point_according_to_direction p (opposite_direction_of dir) shift2 in
    max (check_point p1) (check_point p2)

let check_line_for_fork_patterns (g: game) (pl: player) (p: point) (dir: direction) (score: int) : int =
    match score with
    | s when s = score_3_2 -> check_line_for_fork_pattern_1 g pl p dir
    | _ -> 0

let score_position (g: game) (pl: player) (index: int) (dirs: direction list) : int * ((int * direction) list) * int =
    let point = point_of_index g index |> Option.get in
    let rec score_position' dirs score_accum indicies_accum fork_patterns_count =
        match dirs with
        | [] -> (score_accum, indicies_accum, fork_patterns_count)
        | d :: rest ->
            let score, p1, p2 = score_line g pl point d in
            let pc = check_line_for_fork_patterns g pl point d score in
            let points = get_line_points p1 p2 in
            let processed_indicies = points |> List.map (fun p ->
                let i = index_of_point g p |> Option.get in
                (i, d)
            )
            in
            score_position' rest (score + score_accum) (indicies_accum @ processed_indicies) (fork_patterns_count + pc)
    in
    (* TODO: надо здесь рассчитывать угрозы вилок. Брать fork_patterns_count, учитывать линию 4+1
             если набралось два пункта, значит угроза есть
     *)
    score_position' dirs 0 [] 0

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

let score_board_easy (g: game) (pl: player) : int =
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
    let rec score_board' indexes accum fork_patterns_count =
        match indexes with
        | [] -> (accum, fork_patterns_count)
        | i :: rest ->
            match !dir_arr.(i) with
            | None ->
                score_board' rest accum fork_patterns_count
            | Some dirs ->
                let score, processed_indicies, pc = score_position g pl i dirs in
                (* NOTE: use string_of_int_direction_list to view processed_indicies: *)
                (*       print_endline ((string_of_int i) ^ "i: " ^
                                       (Common.string_of_int_direction_list processed_indicies)); *)
                actualize_dir_array processed_indicies;
                score_board' rest (accum + score) (fork_patterns_count + pc)
    in
    let final_score, fork_patterns_count = score_board' indicies 0 0 in
    let score_for_forks =
        match fork_patterns_count with
        | pc when pc >= 2 -> score_fork
        | _ -> 0
    in
    final_score + score_for_forks


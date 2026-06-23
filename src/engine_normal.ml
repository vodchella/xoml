open Common
open Patterns


let score_of_pattern_kind = function
    | PAT50   -> score_insta_win
    | PAT50H  -> score_inevitable_win
    | PAT42   -> score_inevitable_win
    | PAT33L  -> score_3_2
    | PAT33R  -> score_3_2
    | PAT32   -> score_3_2
    | PAT41L  -> score_4_1
    | PAT41R  -> score_4_1
    | PAT41H1 -> score_4_1
    | PAT41H2 -> score_4_1
    | PAT41H3 -> score_4_1
    | PAT31L  -> score_3_1
    | PAT31R  -> score_3_1
    | PAT31H1 -> score_3_1
    | PAT31H2 -> score_3_1
    | PAT22   -> score_2_2
    | PAT21L  -> score_2_1
    | PAT21R  -> score_2_1

let forks : (pattern_kind * pattern_kind) list =
    [ (PAT41H1, PAT33L)
    ; (PAT41H1, PAT33R)
    ; (PAT41H2, PAT33L)
    ; (PAT41H2, PAT33R)
    ; (PAT41H3, PAT33L)
    ; (PAT41H3, PAT33R)
    ; (PAT33L,  PAT33R)
    ; (PAT41L,  PAT41R)
    ; (PAT41H1, PAT41H2)
    ; (PAT41H1, PAT41H3)
    ; (PAT41H2, PAT41H1)
    ; (PAT41H2, PAT41H3)
    ; (PAT41H3, PAT41H1)
    ; (PAT41H3, PAT41H2)
    ]

let has_forks (pt_kinds : (pattern_kind * int list) list) : bool =
    List.exists
        (fun (p1, p2) ->
            match List.assoc_opt p1 pt_kinds, List.assoc_opt p2 pt_kinds with
            | Some ind1, Some ind2 -> indices_intersection_size ind1 ind2 <= 1
            | _ -> false
        )
        forks

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

let score_board_normal (g: game) (pl: player) : int =
    let indicies = get_occupied_indices g pl        in
    let dir_arr  = ref (init_dirs_array g indicies) in
    let actualize_dir_array (indicies_and_dirs_to_remove: (int * direction) list) =
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
    let rec loop indexes result kinds_accum =
        match indexes with
        | [] -> result, kinds_accum
        | i :: rest ->
            match !dir_arr.(i) with
            | None -> loop rest result kinds_accum
            | Some dirs ->
                let pnt               = point_of_index g i |> Option.get in
                let kinds             = pattern_kinds_at_point g dirs pnt pl in
                let scores            = kinds  |> List.map (fun k -> score_of_pattern_kind (fst k)) in
                let score             = scores |> List.fold_left ( + ) 0 in
                let visited_idxs      = kinds  |> List.map (fun k -> snd k) in
                let visited_idxs_flat = visited_idxs |> List.concat_map (fun (ints, dir) -> List.map (fun i -> (i, dir)) ints) in
                actualize_dir_array visited_idxs_flat;
                loop rest (result + score) (kinds_accum @ kinds)
    in
    let score, kinds      = loop indicies 0 [] in
    let kind_without_dirs = kinds |> List.map (fun (k, (ints, _)) -> (k, ints)) in
    score +
    if has_forks kind_without_dirs then score_inevitable_win else 0

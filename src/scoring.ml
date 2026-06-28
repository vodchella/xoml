open Common
open Patterns


let score_of_pattern_kind = function
    | PAT50   -> score_insta_win
    | PAT42   -> score_4_2
    | PAT50H  -> score_inevitable_win
    | PAT33L  -> score_3_2
    | PAT33R  -> score_3_2
    | PAT33H1 -> score_3_2
    | PAT33H2 -> score_3_2
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

let simple_four_kinds =
    [ PAT41L
    ; PAT41R
    ; PAT41H1
    ; PAT41H2
    ; PAT41H3
    ]

let open_three_kinds =
    [ PAT33L
    ; PAT33R
    ; PAT33H1
    ; PAT33H2
    ]

(* simple_four + simple_four *)
(* simple_four + open_three *)
(* open_three  + open_three *)
let forks : (pattern_kind * pattern_kind) list =
    let cross xs ys =
        xs |> List.concat_map (fun x -> ys |> List.map (fun y -> (x, y)))
    in
    let pairs_with_self xs =
        let rec loop acc = function
            | [] ->
                List.rev acc
            | x :: rest ->
                let pairs = (x, x) :: (rest |> List.map (fun y -> (x, y))) in
                loop (pairs @ acc) rest
        in
        loop [] xs
    in
    pairs_with_self simple_four_kinds
    @ cross simple_four_kinds open_three_kinds
    @ pairs_with_self open_three_kinds

let has_forks (pt_kinds : (pattern_kind * int list) list) : bool =
    let entries = List.mapi (fun i (kind, indices) -> (i, kind, indices)) pt_kinds       in
    let fork_matches (fk1, fk2) (_, kind1, _) (_, kind2, _) = kind1 = fk1 && kind2 = fk2 in
    let different_entries (i1, _, _) (i2, _, _) = i1 <> i2                               in
    let fork_indices_match (_, _, indices1) (_, _, indices2) =
        indices_intersection_size indices1 indices2 <= 1
    in
    let fork_entries_match fork entry1 entry2 =
        fork_matches fork entry1 entry2
        && different_entries entry1 entry2
        && fork_indices_match entry1 entry2
    in
    forks
    |> List.exists
        (fun fork ->
            entries |>
            List.exists (fun entry1 -> entries |> List.exists (fork_entries_match fork entry1)))

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

let score_board
        (g: game)
        (pl: player)
        (ptk_infos: pattern_kind_info list array)
    : int * ((pattern_kind * int list) list)
    =
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
        | []        -> result, kinds_accum
        | i :: rest ->
            match !dir_arr.(i) with
            | None -> loop rest result kinds_accum
            | Some _ ->
                let infos             = ptk_infos.(i)                                                                          in
                let scores            = infos        |> List.map (fun k -> score_of_pattern_kind (fst k))                      in
                let score             = scores       |> List.fold_left ( + ) 0                                                 in
                let visited_idxs      = infos        |> List.map (fun k -> snd k)                                              in
                let visited_idxs_flat = visited_idxs |> List.concat_map (fun (ints, dir) -> List.map (fun i -> (i, dir)) ints) in
                actualize_dir_array visited_idxs_flat;
                loop rest (result + score) (kinds_accum @ infos)
    in
    let score, infos      = loop indicies 0 []                                  in
    let kind_without_dirs = infos |> List.map (fun (k, (ints, _)) -> (k, ints)) in
    (score + if has_forks kind_without_dirs then score_inevitable_win else 0), kind_without_dirs


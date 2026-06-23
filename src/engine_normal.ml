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
    | PAT12   -> score_1_2
    | PAT11L  -> score_1_1
    | PAT11R  -> score_1_1

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

let has_forks (pt_kinds : (pattern_kind * point list) list) : bool =
    List.exists
        (fun (p1, p2) ->
            match List.assoc_opt p1 pt_kinds, List.assoc_opt p2 pt_kinds with
            | Some points1, Some points2 ->
                points_intersection_size points1 points2 <= 1
            | _ ->
                false
        )
        forks

let score_board_normal (g: game) (pl: player) : int =
    let indicies = get_occupied_indices g pl in
    let points   = indicies |> List.map (fun i -> point_of_index g i |> Option.get) in
    let kinds    = points   |> List.map (fun p -> pattern_kinds_at_point g working_dirs p pl) |> List.flatten in
    let scores   = kinds    |> List.map (fun k -> score_of_pattern_kind (fst3 k)) in
    let score    = scores   |> List.fold_left ( + ) 0 in
    score +
    if has_forks (pairs_of_triples kinds) then score_inevitable_win else 0

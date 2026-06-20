open Common


type pattern_kind =
    | PAT50
    | PAT42
    | PAT41L
    | PAT41R
    | PAT41H1
    | PAT41H2
    | PAT41H3
    | PAT32
    | PAT31L
    | PAT31R
    | PAT31H1
    | PAT31H2
    | PAT22
    | PAT21L
    | PAT21R
    | PAT12
    | PAT11L
    | PAT11R
type pattern_point = point * bool
type pattern =
    { rel_points : pattern_point array
    ; kind       : pattern_kind
    ; dir        : direction
    }

let pattern_kinds =
    [ PAT50
    ; PAT42
    ; PAT41L
    ; PAT41R
    ; PAT41H1
    ; PAT41H2
    ; PAT41H3
    ; PAT32
    ; PAT31L
    ; PAT31R
    ; PAT31H1
    ; PAT31H2
    ; PAT22
    ; PAT21L
    ; PAT21R
    ; PAT12
    ; PAT11L
    ; PAT11R
    ]
let pattern_dirs = working_dirs |> Array.of_list

let string_of_pattern_kind = function
    | PAT50   -> "PAT50"
    | PAT42   -> "PAT42"
    | PAT41L  -> "PAT41L"
    | PAT41R  -> "PAT41R"
    | PAT41H1 -> "PAT41H1"
    | PAT41H2 -> "PAT41H2"
    | PAT41H3 -> "PAT41H3"
    | PAT32   -> "PAT32"
    | PAT31L  -> "PAT31L"
    | PAT31R  -> "PAT31R"
    | PAT31H1 -> "PAT31H1"
    | PAT31H2 -> "PAT31H2"
    | PAT22   -> "PAT22"
    | PAT21L  -> "PAT21L"
    | PAT21R  -> "PAT21R"
    | PAT12   -> "PAT12"
    | PAT11L  -> "PAT11L"
    | PAT11R  -> "PAT11R"

let pattern_string_of_pattern_kind = function
    | PAT50   -> "*****"
    | PAT42   -> ".****."
    | PAT41L  -> ".****"
    | PAT41R  -> "****."
    | PAT41H1 -> "*.***"
    | PAT41H2 -> "**.**"
    | PAT41H3 -> "***.*"
    | PAT32   -> ".***."
    | PAT31L  -> ".***"
    | PAT31R  -> "***."
    | PAT31H1 -> "*.**"
    | PAT31H2 -> "**.*"
    | PAT22   -> ".**."
    | PAT21L  -> ".**"
    | PAT21R  -> "**."
    | PAT12   -> ".*."
    | PAT11L  -> ".*"
    | PAT11R  -> "*."

let score_of_pattern = function
    | PAT50   -> score_insta_win
    | PAT42   -> score_win * 2
    | PAT41L  -> score_4_1
    | PAT41R  -> score_4_1
    | PAT41H1 -> score_4_1
    | PAT41H2 -> score_4_1
    | PAT41H3 -> score_4_1
    | PAT32   -> score_3_2
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

let string_of_pattern_values (kind: pattern_kind) (dir: direction) : string =
    let dir  = string_of_direction dir     in
    let kind = string_of_pattern_kind kind in
    kind ^ "_" ^ dir

let pattern_required_points_count (ptrn: pattern) : int =
    Array.fold_left
        (fun acc (_, required) -> acc + if required then 1 else 0)
        0
        ptrn.rel_points

let pattern_generate (kind: pattern_kind) (dir: direction) : pattern =
    let str  = pattern_string_of_pattern_kind kind in
    let step = relative_point_of_direction dir     in
    let base_index =
        match String.index_opt str '*' with
        | Some i -> i
        | None -> failwith "Pattern has no base point"
    in
    let rel_points =
        Array.init
            (String.length str)
            (fun i ->
                let offset = i - base_index in
                let ch = str.[i] in
                ( { x = step.x * offset
                  ; y = step.y * offset
                  }
                , ch = '*'
                )
            )
    in
    { rel_points
    ; kind
    ; dir
    }

let patterns = Array.concat
    (List.map
         (fun kind -> Array.map (fun dir -> pattern_generate kind dir) pattern_dirs)
         pattern_kinds)
let patterns_list = patterns |> Array.to_list

let pattern_find (kind: pattern_kind) (dir: direction) : pattern =
    let matches =
        patterns_list
        |> List.filter (fun pattern -> pattern.kind = kind && pattern.dir = dir)
    in
    match matches with
    | [ pattern ] -> pattern
    | [] -> failwith ("Pattern " ^ (string_of_pattern_values kind dir) ^ " not found")
    | _  -> failwith ("Multiple " ^ (string_of_pattern_values kind dir) ^ " patterns found")

let pattern_at_point (g: game) (pnt: point) (pl: player) (ptrn: pattern) : point list * bool =
    let rec loop i res_points =
        if i >= Array.length ptrn.rel_points then
            res_points, true
        else
            let elem     = ptrn.rel_points.(i) in
            let rel_pnt  = fst elem in
            let required = snd elem in
            let point    = { x = pnt.x + rel_pnt.x; y = pnt.y + rel_pnt.y } in
            match index_of_point g point with
            | Some index -> (
                let pl_opt = g.board.(index) in
                match pl_opt with
                | Some pl' -> (
                    if required && pl' == pl then (
                        loop (i + 1) (point :: res_points)
                    ) else [], false
                )
                | None -> (
                    if not required then loop (i + 1) res_points
                    else [], false
                )
            )
            | None -> [], false
    in
    if point_is_valid g pnt then (
        loop 0 []
    ) else [], false


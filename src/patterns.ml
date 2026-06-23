open Common


type pattern_kind =
    | PAT50
    | PAT50H   (* TODO: are we realy need it? *)
    | PAT42
    | PAT33L
    | PAT33R
    | PAT32
    | PAT41L
    | PAT41R
    | PAT41H1
    | PAT41H2
    | PAT41H3
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

(* INFO: it MUST be ordered by scores like in score_of_pattern_kind *)
let pattern_kinds =
    [ PAT50
    ; PAT50H
    ; PAT42
    ; PAT33L
    ; PAT33R
    ; PAT32
    ; PAT41L
    ; PAT41R
    ; PAT41H1
    ; PAT41H2
    ; PAT41H3
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
    | PAT50H  -> "PAT50H"
    | PAT42   -> "PAT42"
    | PAT33L  -> "PAT33L"
    | PAT33R  -> "PAT33R"
    | PAT32   -> "PAT32"
    | PAT41L  -> "PAT41L"
    | PAT41R  -> "PAT41R"
    | PAT41H1 -> "PAT41H1"
    | PAT41H2 -> "PAT41H2"
    | PAT41H3 -> "PAT41H3"
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
    | PAT50H  -> "*.***.*"
    | PAT42   -> ".****."
    | PAT33L  -> "..***."
    | PAT33R  -> ".***.."
    | PAT32   -> ".***."
    | PAT41L  -> ".****"
    | PAT41R  -> "****."
    | PAT41H1 -> "*.***"
    | PAT41H2 -> "**.**"
    | PAT41H3 -> "***.*"
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
let patterns_all = patterns     |> Array.to_list
let patterns_sw  = patterns_all |> List.filter (fun pattern -> pattern.dir = SW)
let patterns_s   = patterns_all |> List.filter (fun pattern -> pattern.dir = S)
let patterns_se  = patterns_all |> List.filter (fun pattern -> pattern.dir = SE)
let patterns_e   = patterns_all |> List.filter (fun pattern -> pattern.dir = E)

let patterns_of_dir = function
    | SW -> patterns_sw
    | S  -> patterns_s
    | SE -> patterns_se
    | E  -> patterns_e
    | _  -> failwith "unreachable: patterns_of_dir"

let pattern_find (kind: pattern_kind) (dir: direction) : pattern =
    let matches =
        patterns_all
        |> List.filter (fun pattern -> pattern.kind = kind && pattern.dir = dir)
    in
    match matches with
    | [ pattern ] -> pattern
    | [] -> failwith ("Pattern  " ^ (string_of_pattern_values kind dir) ^ " not found")
    | _  -> failwith ("Multiple " ^ (string_of_pattern_values kind dir) ^ " patterns found")

let is_pattern_at_point
        (g:    game)
        (pnt:  point)
        (pl:   player)
        (ptrn: pattern)
    : point list * bool * int
    =
    let rec loop i res_points =
        if i >= Array.length ptrn.rel_points then
            res_points, true, -1
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
                    ) else [], false, i
                )
                | None -> (
                    if not required then loop (i + 1) res_points
                    else [], false, i
                )
            )
            | None -> [], false, i
    in
    if point_is_valid g pnt then (
        loop 0 []
    ) else [], false, -1

let pattern_kind_at_point_and_dir
        (g:   game)
        (pnt: point)
        (pl:  player)
        (dir: direction)
    : (pattern_kind * point list * direction) option
    =
    let rec loop ptrns =
        match ptrns with
        | [] -> None
        | ptrn :: tail -> (
            match is_pattern_at_point g pnt pl ptrn with
            | pnts, true, _ -> Some (ptrn.kind, pnts, dir)
            | _             -> loop tail
        )
    in
    loop (patterns_of_dir dir)

let pattern_kinds_at_point
        (g:            game)
        (allowed_dirs: direction list)
        (pnt:          point)
        (pl:           player)
    : (pattern_kind * point list * direction) list
    =
    List.filter_map
        (fun dir -> pattern_kind_at_point_and_dir g pnt pl dir)
        allowed_dirs


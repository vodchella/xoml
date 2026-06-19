open Common


type pattern_kind =
    | PAT50
    | PAT42
type pattern_point = point * bool
type pattern =
    { rel_points : pattern_point array
    ; kind       : pattern_kind
    ; dir        : direction
    }

let patterns : pattern array = [|
    (* XXXXX *)
    { rel_points = [|
          ({ x = 0; y = 0 }, true);
          ({ x = 1; y = 0 }, true);
          ({ x = 2; y = 0 }, true);
          ({ x = 3; y = 0 }, true);
          ({ x = 4; y = 0 }, true);
      |]
    ; kind = PAT50
    ; dir  = E
    };
    { rel_points = [|
          ({ x = 0; y = 0 }, true);
          ({ x = 1; y = 1 }, true);
          ({ x = 2; y = 2 }, true);
          ({ x = 3; y = 3 }, true);
          ({ x = 4; y = 4 }, true);
      |]
    ; kind = PAT50
    ; dir  = SE
    };
    { rel_points = [|
          ({ x = 0; y = 0 }, true);
          ({ x = 0; y = 1 }, true);
          ({ x = 0; y = 2 }, true);
          ({ x = 0; y = 3 }, true);
          ({ x = 0; y = 4 }, true);
      |]
    ; kind = PAT50
    ; dir  = S
    };
    { rel_points = [|
          ({ x =  0; y = 0 }, true);
          ({ x = -1; y = 1 }, true);
          ({ x = -2; y = 2 }, true);
          ({ x = -3; y = 3 }, true);
          ({ x = -4; y = 4 }, true);
      |]
    ; kind = PAT50
    ; dir  = SW
    };
|]

let string_of_pattern_kind = function
    | PAT50 -> "PAT50"
    | PAT42 -> "PAT42"

let pattern_required_points_count (ptrn: pattern) =
    Array.fold_left
        (fun acc (_, required) -> acc + if required then 1 else 0)
        0
        ptrn.rel_points

let pattern_at_point (g: game) (pnt: point) (pl: player) (ptrn: pattern) : point list * bool =
    let rec loop i res_points =
        if i >= Array.length ptrn.rel_points then
            res_points, true
        else
            let elem     = ptrn.rel_points.(i) in
            let rel_pnt  = fst elem in
            let required = snd elem in
            let point    = { x = pnt.x + rel_pnt.x; y = pnt.y + rel_pnt.y } in
            Logger.write g (move_str_of_point g point);
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


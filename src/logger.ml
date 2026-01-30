open Common


let timestamp () : string =
    let open Unix in
    let t = time () in
    let tm = localtime t in
    let ms = int_of_float ((t *. 1000.) -. (floor t *. 1000.)) in
    Printf.sprintf "[%04d-%02d-%02d %02d:%02d:%02d.%03d]"
        (tm.tm_year + 1900)
        (tm.tm_mon + 1)
        tm.tm_mday
        tm.tm_hour
        tm.tm_min
        tm.tm_sec
        ms

let create (g: game) : game =
    let ch = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o666 "xoml.log" in
    { g with log_file = Some ch }

let write (g: game) (s: string) : unit =
    match g.log_file with
    | Some ch ->
        let ts = timestamp () in
        output_string ch ts;
        output_string ch "   ";
        output_string ch s;
        output_char ch '\n';
        flush ch
    | None ->
        ()

let write_game_move (g: game) (pl: player) (move_str: string) : unit =
    match g.log_moves with
    | true  -> write g ("let g = Engine.apply_move g " ^ (string_of_player pl) ^ " \"" ^ move_str ^ "\" in"); ()
    | false -> ()

let write_game_move_by_index (g: game) (pl: player) (index: int) : unit =
    write_game_move g pl (move_str_of_index g index)


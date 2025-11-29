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

let create (g : game) : game =
    let ch = open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o666 "xoml.log" in
    { g with log_file = Some ch }

let write (g : game) (s : string) : unit =
    match g.log_file with
    | Some ch ->
        let ts = timestamp () in
        output_string ch ts;
        output_char ch ' ';
        output_string ch s;
        output_char ch '\n';
        flush ch
    | None ->
        ()


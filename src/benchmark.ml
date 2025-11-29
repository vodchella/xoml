open Common


let bench_game_fn ?(count = 1000000) (g : game) (name : string) (fn : game -> 'a) : unit =
    let t0 = Unix.gettimeofday () in
    for _ = 1 to count do
        ignore (fn g)
    done;
    let t1 = Unix.gettimeofday () in
    let dt = t1 -. t0 in
    Logger.write g (Printf.sprintf "bench %s (%d runs): %.6f sec" name count dt)


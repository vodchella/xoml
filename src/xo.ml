open Common


let rec main_loop (g: game) =
    Board.draw ();
    Board.print_prompt g;
    let input =
        read_line ()
        |> String.trim
        |> String.lowercase_ascii
        |> Input.of_string
    in
    match input with
    | Invalid invalid_input ->
        let g' = { g with last_tip = invalid_input } in
        (main_loop[@tailcall]) g'
    | Move move ->
        let g' = { g with last_tip = "Your move: " ^ move } in
        (main_loop[@tailcall]) g'
    | Help ->
        let g' = { g with last_tip = inital_tip } in
        (main_loop[@tailcall]) g'
    | Quit -> ()

let main () =
    screen_clear ();
    (main_loop[@tailcall]) inital_game
let () = main ()


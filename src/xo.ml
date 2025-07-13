open Common

type input_result =
    | Move of string
    | Invalid of string
    | Help
    | Quit


(* Utility functions *)

let move_is_valid_input_str str =
    match String.length str with
    | 2 ->
        let c1 = String.get str 0 in
        let c2 = String.get str 1 in
        let is_letter c =
            let code = chrc c in
                (code >= chrc 'A' && code <= chrc 'Z') ||
                (code >= chrc 'a' && code <= chrc 'z')
        in
        let is_digit c =
            let code = chrc c in
                code >= chrc '0' && code <= chrc '9'
        in
        is_letter c1 && is_digit c2
    | _ -> false

let process_input = function
    | "q" | "e" | "quit" | "exit" -> Quit
    | "h" | "help" -> Help
    | s when (move_is_valid_input_str s) -> Move s
    | s -> Invalid ("Invalid move: " ^ s)


(* Main functions *)

let rec main_loop (g: game) =
    Board.draw ();
    Board.print_prompt g;
    let input =
        read_line ()
        |> String.trim
        |> String.lowercase_ascii
        |> process_input
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


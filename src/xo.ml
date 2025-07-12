let printf = Printf.printf
let chrc   = Char.code

type input_result =
    | Move of string
    | Invalid of string
    | Quit

type game =
    { last_tip : string
    ; board    : int list
    }

let board_width   = 10
let board_height  = 10
let input_vmargin = board_height + 5
let input_prompt  = "?> "
let ascii_esc     = "\x1b"
let inital_tip    = "Type your move (e.g. 'E4') or 'Q' to exit"
let inital_game   =
    { last_tip = inital_tip
    ; board    = []
    }


(* Utility functions *)

let screen_clear () =
    Sys.command "clear"
    |> ignore

let cursor_move row col =
    printf "%s[%d;%dH%!" ascii_esc row col
    |> ignore

let print_at str row col =
    cursor_move row col;
    printf "%s" str
    |> ignore

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


(* Board drawing functions *)

let board_make_sparse_row count make_chars_fn =
    String.init count make_chars_fn
    |> String.to_seq
    |> Seq.map (fun ch -> String.make 1 ch ^ " ")
    |> Seq.fold_left ( ^ ) ""

let board_make_column_headers columns_count =
    "   " ^ board_make_sparse_row columns_count (fun i -> Char.chr (chrc 'A' + i))

let board_make_empty_row columns_count =
    board_make_sparse_row columns_count (fun i -> '.')

let board_make_body rows_count row  =
    let lines = List.init rows_count (fun i ->
        let index = rows_count - 1 - i in
        let index_str = string_of_int index in
        index_str ^ "  " ^ row ^ " " ^ index_str ^ "\n")
    in
    String.concat "" lines

let board_draw () =
    let headers    =  board_make_column_headers  board_width      in
    let row        =  board_make_empty_row       board_width      in
    let board_body =  board_make_body            board_height row in
    printf "\n%s\n" headers;
    printf "%s"     board_body;
    printf "%s"     headers
    |> ignore


(* Main functions *)

let process_input = function
    | "q" | "e" | "quit" | "exit" -> Quit
    | s when (move_is_valid_input_str s) -> Move s
    | s -> Invalid ("Invalid move: " ^ s)

let rec main_loop (g: game) =
    screen_clear ();
    board_draw ();
    print_at (g.last_tip) input_vmargin 1;
    print_at input_prompt (input_vmargin + 1) 1;
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
    | Quit -> ()

let main () = (main_loop[@tailcall]) inital_game
let () = main ()


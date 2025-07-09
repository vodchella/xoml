let printf = Printf.printf

type input_result =
    | Move of string
    | Invalid of string
    | Quit

type game = {
    last_tip : string
}

let board_width   = 10
let board_height  = 10
let input_vmargin = board_height + 5
let input_prompt  = "?> "
let ascii_esc     = "\x1b"
let inital_tip    = "Type your move (e.g.: 'E4') or ['q' | 'quit' | 'exit'] to exit"
let inital_game   = {
    last_tip = inital_tip
}


(* Utility functions *)

let screen_clear () =
    Sys.command "clear"
    |> ignore

let cursor_move row col =
    printf "%s[%d;%dH%!" ascii_esc row col
    |> ignore

let print_at (str: string) row col =
    cursor_move row col;
    printf "%s" str
    |> ignore

let move_is_valid_input_str str =
    match String.length str with
    | 2 ->
        let c1 = String.get str 0 in
        let c2 = String.get str 1 in
        let is_letter c =
          let code = Char.code c in
          (code >= Char.code 'A' && code <= Char.code 'Z') ||
          (code >= Char.code 'a' && code <= Char.code 'z')
        in
        let is_digit c =
          let code = Char.code c in
          code >= Char.code '0' && code <= Char.code '9'
        in
        is_letter c1 && is_digit c2
    | _ -> false


(* Board drawing functions *)

let board_make_sparse_row make_chars_fn count =
    String.init make_chars_fn count
    |> String.to_seq
    |> Seq.map (fun ch -> String.make 1 ch ^ " ")
    |> Seq.fold_left ( ^ ) ""

let board_make_column_headers columns_count =
    "   " ^ board_make_sparse_row columns_count (fun i -> Char.chr (Char.code 'A' + i))

let board_make_empty_row columns_count =
    board_make_sparse_row columns_count (fun i -> '.')

let board_make_board_body row rows_count =
    let lines = List.init rows_count (fun i ->
        let index = rows_count - 1 - i in
        let index_str = string_of_int index in
        index_str ^ "  " ^ row ^ " " ^ string_of_int index ^ "\n")
    in
    String.concat "" lines

let board_draw () =
    let headers    =  board_make_column_headers  board_width in
    let row        =  board_make_empty_row       board_width in
    let board_body =  board_make_board_body      row board_height in
    printf "\n%s\n" headers;
    printf "%s"     board_body;
    printf "%s"     headers
    |> ignore


(* Main functions *)

let process_input str =
    match str with
    | s when (move_is_valid_input_str s) -> Move str
    | "q" | "quit" | "exit" -> Quit
    | _   -> Invalid ("Invalid move: " ^ str)

let rec input_loop (g: game) =
    screen_clear ();
    board_draw ();
    print_at (g.last_tip) input_vmargin 1;
    print_at input_prompt (input_vmargin + 1) 1;
    flush stdout;
    let input = read_line () in
    match process_input (String.trim input |> String.lowercase_ascii) with
    | Invalid invalid_input ->
        let new_game = { last_tip = invalid_input } in
        input_loop new_game
    | Move move ->
        let new_game = { last_tip = "Your move: " ^ move } in
        input_loop new_game
    | Quit -> ()

let () =
    input_loop inital_game


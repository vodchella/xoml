let printf = Printf.printf

let board_width  = 10
let board_height = 10
let ascii_esc    = "\x1b"


let screen_clear () =
    Sys.command "clear"
    |> ignore


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


(* Main function *)

let () =
    screen_clear ();
    board_draw ()


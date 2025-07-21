open Common


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

let print_symbol_at_point symbol point =
    print_at symbol (point.y + 2) (point.x * 2 + 2)

let make_sparse_row count make_chars_fn =
    String.init count make_chars_fn
    |> String.to_seq
    |> Seq.map (fun ch -> String.make 1 ch ^ " ")
    |> Seq.fold_left ( ^ ) ""

let make_column_headers columns_count =
    "   " ^ make_sparse_row columns_count (fun i -> Char.chr (chrc 'A' + i))

let make_empty_row columns_count =
    make_sparse_row columns_count (fun _ -> sym_none.[0])

let make_body rows_count row  =
    let lines = List.init rows_count (fun i ->
        let index = rows_count - 1 - i in
        let index_str = string_of_int index in
        index_str ^ "  " ^ row ^ " " ^ index_str ^ "\n")
    in
    String.concat "" lines

let apply_move_by_index (g: game) (p: player) index =
    let point = point_of_index g index |> Option.get in
    let move_str = move_str_of_point g point in
    let cell = g.board.(index) in
    let new_state = match p with
        | O -> Waiting
        | X -> Thinking
    in
    let new_tip = match p with
        | X -> "Your move: '" ^ move_str ^ "'. Thinking..."
        | O -> "Computer's move: '" ^ move_str ^ "'. Now it's your turn..."
    in
    match cell with
    | None ->
        g.board.(index) <- Some p;
        let g' = { g with
                   last_tip        = new_tip
                 ; last_move_str   = Some move_str
                 ; last_move_point = Some point
                 ; last_move_index = Some index
                 ; last_player     = Some p
                 ; state           = new_state
                 }
        in
        g'
    | Some _ ->
        let g' = { g with last_tip = "Cell '" ^ move_str ^ "' is occupied" } in
        g'

let apply_move (g: game) (p: player) move_str =
  let point = point_of_move_str g move_str in
  let index = index_of_point g point |> Option.get in
  apply_move_by_index g p index

let size_from_args () =
    let size = ref None in
    let speclist = [
        ("-s", Arg.Int (fun x ->
            if x < 5 || x > 10 then
              raise (Arg.Bad "Value for -s must be between 5 and 10")
            else
              size := Some x),
         "Set size (must be between 5 and 10)")
    ] in

    let usage_msg = "Usage: program_name [-s X]" in
    Arg.parse speclist print_endline usage_msg;

    match !size with
    | Some s -> s
    | None   -> default_board_width

let draw (g: game) =
    let headers    =  make_column_headers  g.board_width      in
    let row        =  make_empty_row       g.board_width      in
    let board_body =  make_body            g.board_height row in
    cursor_move 1 1;
    printf "\n%s\n" headers;
    printf "%s"     board_body;
    printf "%s"     headers
    |> ignore

let symbol_of_cell = function
   | Some X  -> sym_x
   | Some O  -> sym_o
   | None    -> sym_none

let print_last_figure (g: game) =
    match g.last_move_point, g.last_move_index with
    | Some point, Some index ->
        let symbol = symbol_of_cell g.board.(index) in
        print_symbol_at_point symbol point
    | _ -> ()

let clear_space_for_prompt (g: game) =
    print_at empty_row (g.input_vmargin - 1) 1;
    print_at empty_row g.input_vmargin 1;
    print_at empty_row (g.input_vmargin + 1) 1

let print_prompt (g: game) =
    clear_space_for_prompt g;
    print_at (g.last_tip) g.input_vmargin 1;
    print_at input_prompt (g.input_vmargin + 1) 1
    |> ignore

let print_congratulations (g: game) (p: player) =
    clear_space_for_prompt g;
    print_at ("Player " ^ (symbol_of_cell (Some p)) ^ " wins!") g.input_vmargin 1
    |> ignore


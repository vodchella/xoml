open Common

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

let apply_move (g: game) (p: player) move_str =
  let point = point_of_move_str g move_str in
  let i = index_of_point g point in
  let cell = g.board.(i) in
  match cell with
  | N ->
      g.board.(i) <- p;
      let g' = { g with
                 last_tip      = "Your move: '" ^ move_str ^ "'. Thinking..."
               ; last_move_str = move_str
               ; state         = Thinking
               }
      in
      g'
  | X | O ->
      let g' = { g with last_tip = "Cell '" ^ move_str ^ "' is occupied" } in
      g'

let draw (g: game) =
    let headers    =  make_column_headers  g.board_width      in
    let row        =  make_empty_row       g.board_width      in
    let board_body =  make_body            g.board_height row in
    cursor_move 1 1;
    printf "\n%s\n" headers;
    printf "%s"     board_body;
    printf "%s"     headers;
    g.board
    |> Array.iteri (fun i cell ->
       match cell with
       | N -> ()
       | X | O as p ->
           let pnt = point_of_index g i in
           let symbol = match p with
               | X -> sym_x
               | O -> sym_o
               | N -> sym_none (* unreachable, just for avoid warnings *)
           in
           print_at symbol (pnt.y + 2) (pnt.x * 2 + 2))
    |> ignore

let print_prompt(g: game) =
    print_at empty_row (g.input_vmargin - 1) 1;
    print_at empty_row g.input_vmargin 1;
    print_at empty_row (g.input_vmargin + 1) 1;
    print_at (g.last_tip) g.input_vmargin 1;
    print_at input_prompt (g.input_vmargin + 1) 1
    |> ignore

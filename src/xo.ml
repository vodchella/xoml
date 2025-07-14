open Common


let apply_move (g: game) (p: player) move_str =
  let point = point_of_move_str g move_str in
  let i = index_of_point g point in
  let cell = g.board.(i) in
  match cell with
  | N ->
      g.board.(i) <- p;
      let g' = { g with
                 last_tip      = "Your move: " ^ move_str ^ ". Thinking..."
               ; last_move_str = move_str
               ; state         = Thinking
               }
      in
      g'
  | X | O ->
      let g' = { g with last_tip = "Cell " ^ move_str ^ " is occupied" } in
      g'

let calc_next_move (_g: game) (_p: player) =
    Unix.sleep 2;
    "RND"

let rec main_loop (g: game) =
    Board.draw g;
    Board.print_prompt g;
    match g.state with
    | Thinking ->
        let next_move = calc_next_move g O in
        let g' = { g with
                   last_tip = "Computer's move: " ^ next_move ^ ". Now your turn..."
                 ; last_move_str = next_move
                 ; state = Waiting
                 }
        in
        (main_loop[@tailcall]) g'
    | Waiting ->
        let input =
            read_line ()
            |> String.trim
            |> String.uppercase_ascii
            |> Input.of_string g
        in
        match input with
        | Invalid invalid_input ->
            let g' = { g with last_tip = invalid_input } in
            (main_loop[@tailcall]) g'
        | Move move ->
            let g' = apply_move g X move in
            (main_loop[@tailcall]) g'
        | Help ->
            let g' = { g with last_tip = inital_tip } in
            (main_loop[@tailcall]) g'
        | Quit -> ()

let main () =
    let g = initial_game in
    assert (g.board_width  >= 5);
    assert (g.board_width  <= 10);
    assert (g.board_height >= 5);
    assert (g.board_height <= 10);
    screen_clear ();
    (main_loop[@tailcall]) initial_game
let () = main ()


let () =
    Alcotest.run "all_tests" [
        Xoml_test_directions.suite;
        Xoml_test_score_line.suite;
        Xoml_test_board.suite;
    ]


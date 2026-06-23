let () =
    Alcotest.run "all_tests" [
        Xoml_test_board.suite;
        Xoml_test_patterns.suite;
    ]


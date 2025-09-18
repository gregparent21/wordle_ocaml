open OUnit2
open A1.Wordle

let tests = "test suite for Wordle" >::: [
  "init returns expected state" >:: (fun _ ->
      let s = init () in
      let expected_dictionary = BatList.of_enum (BatFile.lines_of "data/dictionary.txt") in
      assert_bool "Checking secret is in dictionary" (List.mem s.secret_word expected_dictionary);
      assert_equal expected_dictionary s.dictionary
    );
]

let _ = run_test_tt_main tests
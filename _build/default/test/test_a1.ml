open OUnit2
open A1.Wordle

let tests =
  "test suite for Wordle"
  >::: [
         ( "init returns expected state" >:: fun _ ->
           let s = init "../data/La.txt" () in
           let expected_dictionary =
             BatList.of_enum (BatFile.lines_of "../data/La.txt")
           in
           assert_bool "Checking secret is in dictionary"
             (List.mem s.secret_word expected_dictionary);
           assert_equal expected_dictionary s.dictionary );
         (*The following are tests regarding accepting/not accepting words *)
         ( "Word in Ta.txt is accepted as a guess" >:: fun _ ->
           let word = "ulans" in
           assert_bool
             "Checking word in 'Ta' but not 'La' dictionary is still accepted"
             (check_word_validity word "../data/La.txt" "../data/Ta.txt") );
         ( "Word not in La.txt or Ta.txt is not accepted as a guess" >:: fun _ ->
           let word = "dcsho" in
           assert_bool "Checking word in neither dictionary is not accepted"
             (check_word_validity word "../data/La.txt" "../data/Ta.txt" = false)
         );
         ( "Word in La.txt is accepted despite differences in case" >:: fun _ ->
           let word = "HouSe" in
           assert_bool "Checking word is accepted despite capitaliziation"
             (check_word_validity word "../data/La.txt" "../data/Ta.txt") );
         (*Checking the guess accepting system *)
         ( "guess is accepeted when it is the secret word, both in the same \
            and different capitalization patterns"
         >:: fun _ ->
           let s = init "../data/La.txt" () in
           let secret = s.secret_word in
           let secret_upper = String.capitalize_ascii secret in
           assert_bool
             "Checking guess is accepted as the secret word when it is correct"
             (check_word secret_upper secret) );
         ( "guess is accepeted when it is the secret word, both in the same \
            and different capitalization patterns"
         >:: fun _ ->
           let s = init "../data/La.txt" () in
           let secret = s.secret_word in

           assert_bool
             "Checking guess is not accepted when its not the secret word"
             (check_word "word_test" secret = false) );
         (* Checking the color system *)
         ( "Evaluate green is fully [Green] when there is a complete match"
         >:: fun _ ->
           let secret = "robot" in
           assert_bool
             "Checking evaluate_green is fully green when provided the secret"
             (evaluate_green "robot" secret 0 []
             = [ Green; Green; Green; Green; Green ]) );
         ( "Evaluate green is partially [Green] when there is a partial match"
         >:: fun _ ->
           let secret = "stair" in
           let guess = "stain" in
           assert_bool
             "Checking evaluate_green is fully green when provided the secret"
             (evaluate_green guess secret 0 []
             = [ Green; Green; Green; Green; White ]) );
           ( "Evaluate yellow letters correctly when there are no green"
         >:: fun _ ->
           let secret = "house" in
           let guess = "match" in
           assert_bool
             "Checking yellow letters match correctly when there are no green"
             (evaluate_colors guess secret
             = [ White; White; White; White; Yellow ]) );
             ( "Evaluate yellow letters correctly when there are some green"
         >:: fun _ ->
           let secret = "house" in
           let guess = "hatch" in
           
           assert_bool
             "Checking yellow letters match correctly when is a Green letter"
             (evaluate_colors guess secret
             = [ Green; White; White; White; White ]) );
       ]

let _ = run_test_tt_main tests

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
           assert_bool
             "Checking expected dictionary equals the correct dictionary"
             (expected_dictionary = s.dictionary) );
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
         ( "Evaluate yellow letters correctly when there is a duplicate \
            character of a present letter"
         >:: fun _ ->
           let secret = "house" in
           let guess = "hatch" in

           assert_bool
             "Checking no yellow letters are present when there is a duplicate \
              of a single Green letter"
             (evaluate_colors guess secret
             = [ Green; White; White; White; White ]) );
         ( "Evaluate only 1 letter is marked yellow when there is duplicates \
            of said character in the secret "
         >:: fun _ ->
           let secret = "hands" in
           let guess = "sassy" in

           assert_bool
             "Checking only 1 letter is marked yellow when there is duplicates \
              of said character in the secret"
             (evaluate_colors guess secret
             = [ Yellow; Green; White; White; White ]) );
         ( "Evaluate that a green letter will ensure a second same letter \
            isn't marked yellow "
         >:: fun _ ->
           let secret = "apple" in
           let guess = "papal" in

           assert_bool
             "Checking that a 2nd character of the same letter won't be marked \
              yellow if there is only 1 instance of it in the secret (eg. two \
              a's in papal, only marked marked for secret = 'apple' )"
             (evaluate_colors guess secret
             = [ Yellow; Yellow; Green; White; Yellow ]) );
         ( "Evaluate all white when completely different set of letters "
         >:: fun _ ->
           let secret = "money" in
           let guess = "stair" in

           assert_bool "Checking all white when letters are entirely different"
             (evaluate_colors guess secret
             = [ White; White; White; White; White ]) );
         ( "Evaluate [count_of_letter] [increment_count] and [decrement_count] \
            methods "
         >:: fun _ ->
           let counts = [ ("A", 3); ("B", 2) ] in
           assert_bool "Checking Correct count of A"
             (count_of_letter "A" counts = 3);
           let counts = increment_count "A" counts in
           assert_bool "Checking correct incremented count of A"
             (count_of_letter "A" counts = 4);
           let counts = increment_count "C" counts in
           assert_bool
             "Checking correct incremented count of a new added letter"
             (count_of_letter "C" counts = 1);
           let counts = decrement_count "C" counts in
           assert_bool "Checking correct decremented count of a letter to 0"
             (count_of_letter "C" counts = 0) );
       ]

let _ = run_test_tt_main tests

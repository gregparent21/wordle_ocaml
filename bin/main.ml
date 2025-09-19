open A1.Wordle


let () =
  Printf.printf
    "\nWelcome to Wordle! This is expected to be played in Dark Mode. \n"

let () =
  ANSITerminal.print_string
    [ ANSITerminal.green; ANSITerminal.on_black ]
    "Green letters signify a correct letter in the correct location\n"

let () =
  ANSITerminal.print_string
    [ ANSITerminal.yellow; ANSITerminal.on_black ]
    "Yellow letters signify a correct letter in the incorrect location\n"

let () =
  Printf.printf
    "Regular white text signify a letter that is not in the word\n\
     You may begin!\n"

let answer = init ()
(* let () = Printf.printf "The secret is %s\n" answer.secret_word *)
let () = print_endline "Would you like to cheat? Enter (y/n):"

(* TODO figure out why I need the ;;*)
let response = read_line ();;

if response = "y" then 
  Printf.printf "The secret is %s\n" answer.secret_word
else if response = "n" then 
  Printf.printf("Good ethics! Please Continue:\n\n")
else 
  Printf.printf "Invalid input. No cheats will be displayed\n\n"

let rec enter_guess secret () =
  let () = print_string "Enter a Guess: " in
  let user_guess = read_line () in
  if
    check_word
      (String.lowercase_ascii secret)
      (String.lowercase_ascii user_guess)
  then true
  else if check_word_validity user_guess then (
    let str = evaluate (user_guess) (secret) in print_endline (letter_to_string str);
    false)
  else (
    print_endline "Invalid Guess, guess again!";
    enter_guess secret ())

let rec play_game secret guess_num =
  if guess_num >= 6 then
    Printf.printf "Game Over!\nThe correct word was %S.\n" secret
  else
    let correct = enter_guess answer.secret_word () in
    if correct then
      Printf.printf "Correct! You won in %d guesses!\n" (guess_num + 1)
    else (
      (* TODO fix guesses/guess for singular *)
      Printf.printf "Incorrect! You have %d guesses remaining!\n"
        (6 - guess_num - 1);
      play_game secret (guess_num + 1))

let () = play_game answer.secret_word 0

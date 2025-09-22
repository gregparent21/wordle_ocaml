open A1.Wordle

(**Prints the introductary sentences and directions for the game*)
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

(**[answer] initializes the game with the dictionary found in the file
   ["data/La.txt"] *)
let answer = init "data/La.txt" ()

(**Prompts the user to cheat and potentially reveal the answer *)

let () =
  print_endline "Would you like to cheat? Enter (y/n):";
  let response = String.lowercase_ascii (read_line ()) in
  match response with
  | "y" -> Printf.printf "The secret is %s\n" answer.secret_word
  | "n" -> Printf.printf "Good ethics! Please Continue:\n\n"
  | _ -> Printf.printf "Invalid input. No cheats will be displayed\n\n"

(**[color_attributes color] is the list of attributes associated with the
   constant of [color]. This is the color of [color] against a black background
*)
let color_attributes color =
  match color with
  | Green -> [ ANSITerminal.green; ANSITerminal.on_black ]
  | Yellow -> [ ANSITerminal.yellow; ANSITerminal.on_black ]
  | White -> [ ANSITerminal.white; ANSITerminal.on_black ]

(** [print_colored_guess guess colors index] prints the letters of [guess]
    according to the color decided by [color_attributes] and [colors], starting
    at position [index]*)
let rec print_colored_guess guess colors index =
  match colors with
  | [] -> print_newline ()
  | h :: t ->
      let character = String.uppercase_ascii (String.make 1 guess.[index]) in
      (* TODO add genAi for String.make *)
      ANSITerminal.print_string (color_attributes h) character;
      print_colored_guess guess t (index + 1)

(**[enter_guess secret ()] prompts the player for a guess, decides colored
   feedback for said guess, and returns [true] if the guess equals [secret],
   [false] otherwise. If there is an invalid input, it calls itself to prompt
   for a new word to be entered *)
let rec enter_guess secret () =
  let () = print_string "Enter a Guess: " in
  let user_guess = String.lowercase_ascii (read_line ()) in
  if
    check_word
      (String.lowercase_ascii secret)
      (user_guess)
  then (
    let colors = evaluate_colors user_guess secret in
    print_colored_guess user_guess colors 0;
    true)
  else if check_word_validity user_guess "data/La.txt" "data/Ta.txt" then (
    let colors = evaluate_colors user_guess secret in
    print_colored_guess user_guess colors 0;
    false)
  else (
    print_endline "Invalid Guess, guess again!";
    enter_guess secret ())

(**[play_game secret guess_num] runs the game loop with the secret word
   [secret]. The plaeyr has at most 6 guesses, which their current guess is kept
   track via [guess_num]. The game ends when the player either guesses the
   correct [secret], or they use all 6 guesses *)
let rec play_game secret guess_num =
  if guess_num >= 6 then
    Printf.printf "Game Over!\nThe correct word was %S.\n" secret
  else
    let correct = enter_guess secret () in
    if correct then
      Printf.printf "Correct! You won in %d guesses!\n" (guess_num + 1)
    else (
      (* TODO fix guesses/guess for singular *)
      Printf.printf "Incorrect! You have %d guesses remaining!\n"
        (6 - guess_num - 1);
      play_game secret (guess_num + 1))

(**Starts the game using [answer.secret_word] as the secret and [0] as the
   starting guess number *)
let () = play_game answer.secret_word 0

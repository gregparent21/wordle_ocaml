open A1.Wordle

let () = print_endline "Hello, World!"
let answer = init ()
let () = Printf.printf "The secret is %s\n " answer.secret_word

let rec enter_guess () =
  let () = print_string "Enter a Guess: " in
  let the_input = read_line () in
  if check_word answer.secret_word (String.lowercase_ascii the_input) then
    print_endline "Correct!"
  else print_endline "Incorrect Guess!"


let rec play_game n = function
  match n with 
  | 6 -> "You Lose"
  | Some int -> 
    enter_guess();
    play_game n+1;


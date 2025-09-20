(**The state variant holds both the secret word and the dictionary of all
   possible words *)

type state = {
  secret_word : string;
  dictionary : string list;
}

let word_lst = BatList.of_enum (BatFile.lines_of "data/La.txt")

let accepted_word_lst =
  word_lst @ BatList.of_enum (BatFile.lines_of "data/Ta.txt")

let () = Random.self_init ()

(** Initialize a new game by choosing a random secret from the selected
    dictionary. The new game has fields secret_word, and dictionary, which are
    said random secret from said dictionary, respectively *)
let init () =
  let secret = List.nth word_lst (Random.int (List.length word_lst)) in
  { secret_word = secret; dictionary = word_lst }

let check_word guess secret = guess = secret
let check_word_validity guess = List.mem guess accepted_word_lst

(** The letter variant assigns a Constant to each position/letter in a guess:
    - Green = correct letter in the correct position
    - Yellow = correct letter in the wrong position
    - White = letter not present in the secret *)

type letter =
  | White
  | Yellow
  | Green

(**Returns the number of occurances of the letter c in the association list
   'counts' *)

let rec count_of_letter c counts =
  match counts with
  | [] -> 0
  | (k, v) :: t -> if c = k then v else count_of_letter c t

(**Takes in a letter 'c' and an association list 'counts' and prepends an
   incremented count to the list*)
let rec increment_count c counts =
  match counts with
  | [] -> [ (c, 1) ]
  | (k, v) :: t ->
      if k = c then (k, v + 1) :: t else (k, v) :: increment_count c t

(**Takes in a letter 'c' and an association list 'counts' and prepends a
   decremented count to the list*)
let rec decrement_count c counts =
  match counts with
  | [] -> []
  | (k, v) :: t ->
      if k = c then if v > 1 then (k, v - 1) :: t else t
      else (k, v) :: decrement_count c t

let rec evaluate_green guess secret index lst =
  (* TODO come back to list reversal here *)
  if index = String.length guess then List.rev lst
  else if guess.[index] = secret.[index] then
    evaluate_green guess secret (index + 1) (Green :: lst)
  else evaluate_green guess secret (index + 1) (White :: lst)

let rec find_count secret index colors counts =
  match colors with
  | [] -> counts
  | h :: t ->
      if h = Green then find_count secret (index + 1) t counts
      else
        find_count secret (index + 1) t (increment_count secret.[index] counts)

let rec find_yellow guess index colors counts acc =
  match colors with
  (* Look into reverse function *)
  | [] -> List.rev acc
  | h :: t -> (
      match h with
      | Green -> find_yellow guess (index + 1) t counts (Green :: acc)
      | White ->
          if count_of_letter guess.[index] counts > 0 then
            let counts1 = decrement_count guess.[index] counts in
            find_yellow guess (index + 1) t counts1 (Yellow :: acc)
          else find_yellow guess (index + 1) t counts (White :: acc)
      | Yellow -> find_yellow guess (index + 1) t counts (Yellow :: acc))

let evaluate_colors guess secret =
  let colors = evaluate_green guess secret 0 [] in
  let counts = find_count secret 0 colors [] in
  find_yellow guess 0 colors counts []

(* TODO Swap out for colored text *)
let rec letter_to_string = function
  | [] -> ""
  | Green :: t -> "🟩" ^ letter_to_string t
  | Yellow :: t -> "🟨" ^ letter_to_string t
  | White :: t -> "⬛" ^ letter_to_string t

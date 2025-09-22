(**[state] variant holds both the secret word and the dictionary of all possible
   words *)

type state = {
  secret_word : string;
  dictionary : string list;
}

(**[word_lst la_txt] is the String list of words in the dictionary file with
   path [la_txt] *)
let word_lst la_txt = BatList.of_enum (BatFile.lines_of la_txt)

(**[accepted_word_lst la_txt ta_txt] is the String list of accepted guesses.
   Formed from the String list of words from [la_txt] file path appended to the
   String list of words in [ta_txt] file path*)
let accepted_word_lst la_txt ta_txt =
  word_lst la_txt @ BatList.of_enum (BatFile.lines_of ta_txt)

let () = Random.self_init ()

(** [init] returns a new game by choosing a random secret from the selected
    dictionary. The new game has fields [secret_word], and [dictionary], which
    are said random secret from said dictionary, respectively *)
let init la_txt () =
  let la_words = word_lst la_txt in
  let secret = List.nth la_words (Random.int (List.length la_words)) in
  { secret_word = secret; dictionary = la_words }

(* (**[check_word guess secret] is true if [guess] is equal to [secret],
   ignoring differences in upper/lower case. False *) otherwise*)
let check_word guess secret =
  String.lowercase_ascii guess = String.lowercase_ascii secret

(**[check_word_validity guess] is true if [guess] is in [accepted_words_lst],
   ignoring differences in case. Returns false otherwise *)
let check_word_validity guess la_txt ta_txt =
  List.mem (String.lowercase_ascii guess) (accepted_word_lst la_txt ta_txt)

(** The [letter] variant assigns a Constant to each position/letter in a guess:
    - [Green] = correct letter in the correct position
    - [Yellow] = correct letter in the wrong position
    - [White] = letter not present in the secret *)

type letter =
  | White
  | Yellow
  | Green

(**[count_of_letter c counts] returns the number of occurances of [c] in the
   association list [counts]. Returns 0 for an empty list*)

let rec count_of_letter c counts =
  match counts with
  | [] -> 0
  | (k, v) :: t -> if c = k then v else count_of_letter c t

(**[increment_count c counts] takes in [c] and an association list, [counts],
   and prepends an incremented count to the list. If [c] isn't in [counts], it
   appends a new key, value tuple starting at value 1*)
let rec increment_count c counts =
  match counts with
  | [] -> [ (c, 1) ]
  | (k, v) :: t ->
      if k = c then (k, v + 1) :: t else (k, v) :: increment_count c t

(**[decrement_count c counts] takes in [c] and an association list [counts] and
   prepends a decremented count to the list*)
let rec decrement_count c counts =
  match counts with
  | [] -> []
  | (k, v) :: t ->
      if k = c then if v > 1 then (k, v - 1) :: t else t
      else (k, v) :: decrement_count c t

(**[evaluate_green guess secret index acc] returns [acc], a list of type letter.
   It iterates through [guess] checking if the letter at position [index] is
   equal to the letter at said [index] in [secret]. It then prepends a Constant
   of type letter to [acc]*)

let rec evaluate_green guess secret index acc =
  (* TODO come back to list reversal here *)
  if index = String.length guess then List.rev acc
  else if guess.[index] = secret.[index] then
    evaluate_green guess secret (index + 1) (Green :: acc)
  else evaluate_green guess secret (index + 1) (White :: acc)

(**[find_count secret index colors acc] returns [acc], an association list of
   [secret]'s remaining letters at positions that are not of constant [Green].
   Used to decide which [White] constants can be upgraded to [Yellow]. *)

let rec find_count secret index colors acc =
  match colors with
  | [] -> acc
  | h :: t ->
      if h = Green then find_count secret (index + 1) t acc
      else find_count secret (index + 1) t (increment_count secret.[index] acc)

(**[find_yellow guess index colors counts acc] is the second iteration over
   [colors], determining which letters at position [index] can be upgraded from
   [White] to [Yellow] if [counts] at the [index]'ed letter is positive. If an
   upgrade occurs, [counts] at said letter is decremented. [find_yellow] returns
   the accumulator of updated colors*)
let rec find_yellow guess index colors counts acc =
  match colors with
  (* Look into reverse function *)
  | [] -> List.rev acc
  | h :: t -> (
      match h with
      | Green -> find_yellow guess (index + 1) t counts (Green :: acc)
      | White ->
          if count_of_letter guess.[index] counts > 0 then
            let updated_count = decrement_count guess.[index] counts in
            find_yellow guess (index + 1) t updated_count (Yellow :: acc)
          else find_yellow guess (index + 1) t counts (White :: acc)
      | Yellow -> find_yellow guess (index + 1) t counts (Yellow :: acc))

(**[evaluate_colors guess secret] is the full Wordle scoring for [guess] against
   [secret]. It first finds the [Green], meaning correct position letters, by
   calling [evaluate_green], and then finds the [Yellow] letters.*)
let evaluate_colors guess secret =
  let colors = evaluate_green guess secret 0 [] in
  let counts = find_count secret 0 colors [] in
  find_yellow guess 0 colors counts []

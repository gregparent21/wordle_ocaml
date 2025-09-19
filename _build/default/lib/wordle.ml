type state = {
  secret_word : string;
  dictionary : string list;
}

let word_lst = BatList.of_enum (BatFile.lines_of "data/La.txt")

let accepted_word_lst =
  word_lst @ BatList.of_enum (BatFile.lines_of "data/Ta.txt")

let () = Random.self_init ()

let init () =
  let secret = List.nth word_lst (Random.int (List.length word_lst)) in
  { secret_word = secret; dictionary = word_lst }

let check_word guess secret = guess = secret
let check_word_validity guess = List.mem guess accepted_word_lst

type letter =
  | White
  | Yellow
  | Green

let letter_count = []

let evaluate guess secret =
  let rec check index lst =
    if index == 5 then List.rev lst
    else if guess.[index] = secret.[index] then
      check (index + 1) (Green :: lst)
    else check (index + 1) (White :: lst)
  in
  check 0 []

  let rec letter_to_string = function
  | [] -> ""
  | Green :: t -> "🟩" ^ letter_to_string t
  | Yellow :: t -> "🟨" ^ letter_to_string t
  | White  :: t -> "⬛" ^ letter_to_string t
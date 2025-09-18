type state = {
  secret_word : string;
  dictionary : string list;
}

let word_lst = BatList.of_enum (BatFile.lines_of "data/dictionary.txt")
let length = List.length word_lst

let init () =
  Random.self_init ();
  let secret = List.nth word_lst (Random.int length) in
  { secret_word = secret; dictionary = word_lst }

let check_word guess secret = guess = secret
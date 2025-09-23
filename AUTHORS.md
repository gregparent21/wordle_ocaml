Author: Gregory Parent, gmp89

Collaborators: 
- I did not collaborate with anyone

GenAI Usage:
- I used ChatGPT when making my [print_colored_guess guess colors index] function in main.ml. I wanted to be able to go through the string and print each letter individually, however, when you call [guess.[index]], this returns a character. However, I wanted it in the form of a String to use the [ANSITerminal.print_string] method. I consulted ChatGPT, which informed me of [String.make], which I used to resolve this problem and convert the characters into Strings of length 1. 

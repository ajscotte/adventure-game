(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string list

type command =
  | Go of object_phrase
  | Quit

exception Empty

exception Malformed

(*Removes all "" from the list to make sure only non "" characters are
  recognized*)
let rec remove_spaces = function
  | [] -> []
  | "" :: b -> remove_spaces b
  | a :: b -> a :: remove_spaces b

(*parse: already described in command.mli*)
let parse str =
  let string_lst = remove_spaces (String.split_on_char ' ' str) in
  let size = List.length string_lst in
  if string_lst = [] then raise Empty
  else if List.hd string_lst = "quit" && size = 1 then Quit
  else if List.hd string_lst = "go" && size > 1 then
    Go (List.tl string_lst)
  else raise Malformed

(* Note: You may introduce new code anywhere in this file. *)

(*t: already described in state.mli*)
type t = {
  current_room : Adventure.room_id;
  visited_rooms : string list;
}

(*init_state: already described in state.mli*)
let init_state adv =
  {
    current_room = Adventure.start_room adv;
    visited_rooms = [ Adventure.start_room adv ];
  }

(*current_room_id: already described in state.mli*)
let current_room_id st = st.current_room

(*visited: already described in state.mli*)
let visited st = st.visited_rooms

(*result: already described in state.mli*)
type result =
  | Legal of t
  | Illegal

(*sees if the exit is in the exit list for teh room*)
let rec find_exit_in_lst ex = function
  | [] -> false
  | a :: b -> if a = ex then true else find_exit_in_lst ex b

(*used to help simplify go function by finding tyhe next room given the
  exit used*)
let next_room ex adv st = Adventure.next_room adv st.current_room ex

(*go: already described in state.mli*)
let go ex adv st =
  let current = st.current_room in
  let exit_lst = Adventure.exits adv current in
  if find_exit_in_lst ex exit_lst then
    Legal
      {
        current_room = next_room ex adv st;
        visited_rooms =
          List.sort_uniq Stdlib.compare
            (next_room ex adv st :: st.visited_rooms);
      }
  else Illegal

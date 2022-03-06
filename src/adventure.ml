(* Note: You may introduce new code anywhere in this file. *)
open Yojson.Basic.Util

type room_id = string

type exit_name = string

exception UnknownRoom of room_id

exception UnknownExit of exit_name

(*definition for a record that can store exit values *)
type exits_val = {
  name : exit_name;
  room_id : room_id;
}

(*definition for record that can stor room values*)
type room = {
  id : string;
  description : string;
  exit_lst : exits_val list;
}

(*uses the room type and start_room to store type t values*)
type t = {
  rooms : room list;
  start_room : room_id;
}
(********************************************************************
   functiopns to parse json files
 ********************************************************************)

(** [exit_of_json json] parses [json] into exit record. Requires: [json]
    is a valid JSON adventure representation. *)
let exit_of_json json =
  {
    name = json |> member "name" |> to_string;
    room_id = json |> member "room id" |> to_string;
  }

(** [room_of_json json] parses [json] into room record. Requires: [json]
    is a valid JSON adventure representation. *)
let room_of_json json =
  {
    exit_lst =
      json |> member "exits" |> to_list |> List.map exit_of_json;
    id = json |> member "id" |> to_string;
    description = json |> member "description" |> to_string;
  }

(********************************************************************
   adventure helper functions
 ********************************************************************)

(** [room_id_helper rooms_lst] uses[rooms_lst] to build a list of room
    ids. Used by room_ids. Requires: [rooms_lst] is a list of the room
    record *)
let rec room_id_helper rooms_lst =
  match rooms_lst with
  | [] -> []
  | a :: b -> a.id :: room_id_helper b

(** [room_des_helper rooms_lst id] uses [rooms_lst] to find description
    of [id] and is used by description. Requires: [rooms_lst] is a list
    of the room record and [id] the room_id in question*)
let rec room_des_helper rooms_lst id =
  match rooms_lst with
  | [] -> raise (UnknownRoom id)
  | a :: b -> if a.id = id then a.description else room_des_helper b id

(** [room_helper rooms_lst id builder] uses[rooms_lst] to search for
    [id] and uses [builder] as a function that builds the appropriate
    exit list. Used by exits and next_rooms. Requires: [rooms_lst] is a
    list of the room record [id] is room_id and [builder] the function
    that builds a specified list*)
let rec room_helper rooms_lst id builder =
  match rooms_lst with
  | [] -> raise (UnknownRoom id)
  | a :: b ->
      if a.id = id then builder a.exit_lst else room_helper b id builder

(** [exit_id_lst_mker] builder for exits.*)
let rec exit_id_lst_mker = function
  | [] -> []
  | a :: b -> a.name :: exit_id_lst_mker b

(** [find_exit_room exits_lst exit_name] uses [exits_lst] to find room
    id of [exit_name] and is used by next_room. Requires: [exits_lst] is
    a list of the exit record and [exit_name] the exit name in question*)
let rec find_exit_room exits_lst exit_name =
  match exits_lst with
  | [] -> raise (UnknownExit exit_name)
  | a :: b ->
      if a.name = exit_name then a.room_id
      else find_exit_room b exit_name

(** [next_room_helper rooms_lst id ex] uses [rooms_lst] to find exit_lst
    of [id] and is used by next_room. Requires: [rooms_lst] is a list of
    the room record and [id] the room_id in question and [ex] the exit
    the function is looking for*)
let rec next_room_helper rooms_lst id ex =
  match rooms_lst with
  | [] -> raise (UnknownRoom id)
  | a :: b ->
      if a.id = id then find_exit_room a.exit_lst ex
      else next_room_helper b id ex

(** [next_room_id_mker] builder for next_rooms.*)
let rec next_room_id_mker rooms_lst =
  match rooms_lst with
  | [] -> []
  | a :: b -> a.room_id :: next_room_id_mker b

(********************************************************************
   required adventure funtions 
 ********************************************************************)

(*from_json function. Already desscribed in adventure.mli*)
let from_json json =
  {
    rooms = json |> member "rooms" |> to_list |> List.map room_of_json;
    start_room = json |> member "start room" |> to_string;
  }

(*start room: already described in adventure.mli*)
let start_room adv = adv.start_room

(*room_ids: already described in adventure.mli*)
let room_ids adv =
  List.sort_uniq Stdlib.compare (room_id_helper adv.rooms)

(*description: already described in adventure.mli*)
let description adv room = room_des_helper adv.rooms room

(*exits: already described in adventure.mli*)
let exits adv room =
  List.sort_uniq Stdlib.compare
    (room_helper adv.rooms room exit_id_lst_mker)

(*next room: already described in adventure.mli*)
let next_room adv room ex = next_room_helper adv.rooms room ex

(*next_rooms: already described in adventure.mli*)
let next_rooms adv room =
  List.sort_uniq Stdlib.compare
    (room_helper adv.rooms room next_room_id_mker)

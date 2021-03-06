(** Representation of dynamic adventure state.

    This module represents the state of an adventure as it is being
    played, including the adventurer's current room, the rooms that have
    been visited, and functions that cause the state to change. *)

(**********************************************************************
 * DO NOT CHANGE THIS FILE
 * It is part of the interface the course staff will use to test your
 * submission.
 **********************************************************************)

type t
(** The abstract type of values representing the game state. *)

val init_state : Adventure.t -> t
(** [init_state a] is the initial state of the game when playing
    adventure [a]. In that state the adventurer is currently located in
    the starting room, and they have visited only that room. *)

val current_room_id : t -> Adventure.room_id
(** [current_room_id st] is the identifier of the room in which the
    adventurer currently is located in state [st]. *)

val visited : t -> Adventure.room_id list
(** [visited st] is a set-like list of the room identifiers the
    adventurer has visited in state [st]. The adventurer has visited a
    room [rm] if their current room location is or has ever been [rm]. *)

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

val go : Adventure.exit_name -> Adventure.t -> t -> result
(** [go exit adv st] is [r] if attempting to go through exit [exit] in
    state [st] and adventure [adv] results in [r]. If [exit] is an exit
    from the adventurer's current room, then [r] is [Legal st'], where
    in [st'] the adventurer is now located in the room to which [exit]
    leads. Otherwise, the result is [Illegal].

    Effects: none. [go] is not permitted to do any printing. *)

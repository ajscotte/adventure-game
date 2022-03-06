open OUnit2
open Game
open Adventure
open Command
open State

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(* These tests demonstrate how to use [cmp_set_like_lists] and [pp_list]
   to get helpful output from OUnit. *)
let cmp_demo =
  [
    ( "order is irrelevant" >:: fun _ ->
      assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
        [ "foo"; "bar" ] [ "bar"; "foo" ] );
    (* Uncomment this test to see what happens when a test case fails.
       "duplicates not allowed" >:: (fun _ -> assert_equal
       ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string) ["foo";
       "foo"] ["foo"]); *)
  ]

(********************************************************************
   End helper functions.
 ********************************************************************)

(* You are welcome to add strings containing JSON here, and use them as
   the basis for unit tests. You can also use the JSON files in the data
   directory as tests. And you can add JSON files in this directory and
   use them, too. *)

(* You should not be testing any helper functions here. Test only the
   functions exposed in the [.mli] files. Do not expose your helper
   functions. See the handout for an explanation. *)

(* TODO: add unit tests for modules below. You are free to reorganize
   the definitions below. Just keep it clear which tests are for which
   modules. *)

(*json file definitions to use in from_json*)
let lonely = Yojson.Basic.from_file "data/lonely_room.json"

let ho_plaza = Yojson.Basic.from_file "data/ho_plaza.json"

(*converting tto Adventure.t for future use for both json files*)
let lonely_t = from_json lonely

let ho_plaza_t = from_json ho_plaza

(*indicators to use in room_ids*)
let lonely_rooms_id = [ "the room" ]

let ho_plaza_rooms = [ "ho plaza"; "health"; "tower"; "nirvana" ]
(********************************************************************
  helper functions for adventure
 ********************************************************************)
(** [start_room_tests name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [start_room input]. *)
let start_room_tests
    (name : string)
    (adv : Adventure.t)
    (expected_output : Adventure.room_id) : test =
  name >:: fun _ -> assert_equal expected_output (start_room adv)

(** [room_ids_tests name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [room_ids input]. *)
let room_ids_tests
    (name : string)
    (adv : Adventure.t)
    (expected_output : room_id list) : test =
  name >:: fun _ ->
  assert (cmp_set_like_lists expected_output (room_ids adv))

(** [description_tests name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [decrition input]. *)
let description_tests
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (description adv room)

(** [exits_tests name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [exits input]. *)
let exits_tests
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (expected_output : exit_name list) : test =
  name >:: fun _ ->
  assert (cmp_set_like_lists expected_output (exits adv room))

(** [next_roomk_tests name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [next_room input]. *)
let next_room_test
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (ex : exit_name)
    (expected_output : room_id) : test =
  name >:: fun _ -> assert_equal expected_output (next_room adv room ex)

(** [next_rooms_tests name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [next_rooms input]. *)
let next_rooms_test
    (name : string)
    (adv : Adventure.t)
    (room : room_id)
    (expected_output : exit_name list) : test =
  name >:: fun _ ->
  assert (cmp_set_like_lists expected_output (next_rooms adv room))
(********************************************************************
  helper function for command
 ********************************************************************)
let parse_test
    (name : string)
    (str : string)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output (parse str)

(********************************************************************
  helper functions for state
 ********************************************************************)

 (** [current_+room_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [current_room input]. *)
let current_room_id_test
    (name : string)
    (st : State.t)
    (expected_output : room_id) : test =
  name >:: fun _ -> assert_equal expected_output (current_room_id st)

(** [visited_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [visited input]. *)
let visited_test
    (name : string)
    (st : State.t)
    (expected_output : room_id list) : test =
  name >:: fun _ ->
  assert (cmp_set_like_lists expected_output (visited st))

(** [go_test name input expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [go input]. *)
let go_test
    (name : string)
    (ex : exit_name)
    (adv : Adventure.t)
    (st : State.t)
    (expected_output : result) : test =
  name >:: fun _ -> assert_equal expected_output (go ex adv st)

let adventure_tests =
  [
    (*simple start room test for json files*)
    start_room_tests "lonely room start" lonely_t "the room";
    start_room_tests "ho plaza start" ho_plaza_t "ho plaza";
    (*room_id tests*)
    room_ids_tests "lonely room ids" lonely_t lonely_rooms_id;
    room_ids_tests "lonely room ids" ho_plaza_t ho_plaza_rooms;
    (*simple description tests*)
    description_tests "lonely room desc" lonely_t "the room"
      "A very lonely room.";
    description_tests "ho plaza desc" ho_plaza_t "ho plaza"
      "You are on Ho Plaza. Cornell Health is to the southwest. The \
       chimes are playing a concert in the clock tower. Someone tries \
       to hand you a quartercard, but you avoid them.";
    description_tests "health desc" ho_plaza_t "health"
      "You are at the entrance to Cornell Health. A sign advertises \
       free flu shots. You briefly wonder how long it would take to \
       get an appointment. Ho Plaza is to the northeast.";
    description_tests "tower desc" ho_plaza_t "tower"
      "You climbed up all 161 steps to the top of McGraw Tower. A \
       Chimesmaster is playing the Jennie McGraw Rag. You feel \
       inspired to ascend higher.";
    description_tests "nirvana desc" ho_plaza_t "nirvana"
      "You have reached a higher level of existence.  There are no \
       more words.";
    (*simple exits tests*)
    exits_tests "lonely exits" lonely_t "the room" [];
    exits_tests "ho plaza exits" ho_plaza_t "ho plaza"
      [
        "southwest";
        "south west";
        "Cornell Health";
        "Gannett";
        "chimes";
        "concert";
        "clock tower";
      ];
    exits_tests "health exits" ho_plaza_t "health"
      [ "northeast"; "north east"; "Ho Plaza" ];
    exits_tests "tower exits" ho_plaza_t "tower"
      [ "down"; "back"; "Ho Plaza"; "higher" ];
    exits_tests "nirvana exits" ho_plaza_t "nirvana" [];
    (*next_room tests*)
    next_room_test "ho plaza southwest" ho_plaza_t "ho plaza"
      "southwest" "health";
    next_room_test "ho plaza south west" ho_plaza_t "ho plaza"
      "southwest" "health";
    next_room_test "ho plaza concert" ho_plaza_t "ho plaza" "concert"
      "tower";
    next_room_test "health Ho Plaza" ho_plaza_t "health" "Ho Plaza"
      "ho plaza";
    next_room_test "tower higher" ho_plaza_t "tower" "higher" "nirvana";
    (*next_rooms tests*)
    next_rooms_test "ho plaza next room" ho_plaza_t "ho plaza"
      [ "health"; "tower" ];
    next_rooms_test "health next room" ho_plaza_t "health"
      [ "ho plaza" ];
    next_rooms_test "tower next room" ho_plaza_t "tower"
      [ "ho plaza"; "nirvana" ];
    next_rooms_test "nirvana next room" ho_plaza_t "nirvana" [];
    next_rooms_test "lonely room next room" lonely_t "the room" [];
  ]

(*All parsing tests*)
let command_tests =
  [
    parse_test "quit command" "quit" Quit;
    parse_test "go one word" "go tower" (Go [ "tower" ]);
    parse_test "go three words" "go south east tower"
      (Go [ "south"; "east"; "tower" ]);
    parse_test "go many words" "go tower plaza happy restaurant"
      (Go [ "tower"; "plaza"; "happy"; "restaurant" ]);
    parse_test "quit spaces" "     quit      " Quit;
    parse_test "go with spaces" "    go    plaza    tower    "
      (Go [ "plaza"; "tower" ]);
      
    (* exception tests*)
    "malformed test" >:: (fun _ -> assert_raises (Malformed) (fun () -> parse "   go"));
    "empty test" >:: (fun _ -> assert_raises (Empty) (fun () -> parse "     "));
  ]

(*defines the initial states for each json file*)
let lonely_state_init = init_state lonely_t

let ho_plaza_state_init = init_state ho_plaza_t

(*extracts State.t from result to use im other functions*)
let extract_t = function
  | Legal t -> t
  | Illegal -> raise (Failure "not extractable")

(*indicators used to help with go functions*)
let go_once = go "southwest" ho_plaza_t ho_plaza_state_init

let go_ho_plaza = go "northeast" ho_plaza_t (extract_t go_once)

let go_health = go "Gannett" ho_plaza_t (extract_t go_ho_plaza)

let go_back_ho = go "Ho Plaza" ho_plaza_t (extract_t go_health)

let go_tower = go "chimes" ho_plaza_t (extract_t go_back_ho)

let go_nirvana = go "higher" ho_plaza_t (extract_t go_tower)

let state_tests =
  [
    (*simple start state*)
    current_room_id_test "current lonely start" lonely_state_init
      "the room";
    current_room_id_test "current ho plaza start" ho_plaza_state_init
      "ho plaza";
    (*Simple visited tests*)
    visited_test "visited lonely start" lonely_state_init [ "the room" ];
    visited_test "visited ho plaza start" ho_plaza_state_init
      [ "ho plaza" ];
    (*simple go test*)
    go_test "illegal output" "south" ho_plaza_t ho_plaza_state_init
      Illegal;
    (*Indirect legal go test simple current state*)
    current_room_id_test "go used once current room" (extract_t go_once)
      "health";
    current_room_id_test "go used twice current room"
      (extract_t go_ho_plaza) "ho plaza";
    current_room_id_test "go used three times current room"
      (extract_t go_health) "health";
    current_room_id_test "go used four times current room"
      (extract_t go_back_ho) "ho plaza";
    current_room_id_test "go used five times current room"
      (extract_t go_tower) "tower";
    current_room_id_test "go used four times current room"
      (extract_t go_nirvana) "nirvana";
    (*Indirect legal go tests visited array*)
    visited_test "first go list" (extract_t go_once)
      [ "ho plaza"; "health" ];
    visited_test "no duplicates" (extract_t go_ho_plaza)
      [ "ho plaza"; "health" ];
    visited_test "going back to health" (extract_t go_health)
      [ "ho plaza"; "health" ];
    visited_test "going back to ho" (extract_t go_back_ho)
      [ "ho plaza"; "health" ];
    visited_test "adding third room" (extract_t go_tower)
      [ "ho plaza"; "health"; "tower" ];
    visited_test "adding fourth room" (extract_t go_nirvana)
      [ "ho plaza"; "health"; "tower"; "nirvana" ];
  ]

let suite =
  "test suite for A2"
  >::: List.flatten [ adventure_tests; command_tests; state_tests ]

let _ = run_test_tt_main suite

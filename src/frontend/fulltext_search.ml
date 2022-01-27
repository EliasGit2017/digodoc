(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Js_of_ocaml
open Js
open Globals
open Data_types
open Utils

open Objects

(* module OrderedSource = struct
   type t = Data_types.file_type
   let compare e1 e2 =
    match e1, e2 with
    | x, y when x = y -> 0
    | ML, _ | DUNE, MAKEFILE -> -1
    | _ -> 1
   end *)
(** Odered source type *)

(* module SourceSet = Set.Make(OrderedSource) *)
(** Set of sources *)

type fulltext_search_state = {
  mutable pattern : string;
  mutable files : file_type;
  mutable is_regex : bool;
  mutable is_case_sensitive : bool;
  mutable last_match_id : int
}
(** State for fulltext search *)

type search_state = 
  | Uninitialized
  | FulltextSearch of fulltext_search_state
  (** Search state type within fulltext search page *)

let search_state = ref Uninitialized
(** Global variable that stores state of fulltext search page *)

let state_of_args args = 
  match List.assoc "search" args with
  | "fulltext" ->
      let state = {
        pattern = "";
        files = ML;
        is_regex = true;
        is_case_sensitive = true;
        last_match_id = 10
      }
      in
      List.iter (fun (key, elt) ->
          match key with
          | "search" -> ()
          | "pattern" -> state.pattern <- decode_query_val elt
          | "files" -> state.files <- file_type_of_string elt
          | "is_regex" -> (match elt with "text" -> state.is_regex <- false | _ -> state.is_regex <- true)
          | "is_case_sensitive" -> (match elt with "aA" -> state.is_case_sensitive <- true | _ -> state.is_case_sensitive <- false)
          | "last_match_id" -> state.last_match_id <- int_of_string elt
          | _ -> raise @@ web_app_error (Printf.sprintf "state_of_args: key %s is not recognised" key)
        )
        args;
      FulltextSearch state
  | s -> raise @@ web_app_error (Printf.sprintf "state_of_args: search type %s is not recognised in fulltext search" s)
(** State_of_args *)

let state_to_args state =
  match state with
  | Uninitialized -> ""
  | FulltextSearch {pattern; files; is_regex; is_case_sensitive; last_match_id} ->
      Printf.sprintf "search=fulltext&pattern=%s&files=%s&is_regex=%s&is_case_sensitive=%s&last_match_id=%d"
        (encode_query_val pattern)
        (file_type_to_string files)
        (if is_regex then "yes" else "text")
        (if is_case_sensitive then "aA" else "no")
        last_match_id
(** [state_to_args state] constructs query string from search state [state] *)

let get_fulltext_search_state () =
  match !search_state with
  | FulltextSearch state -> state
  | _ -> raise @@ web_app_error "get_fulltext_search_state : current state can't be retrieved (may not be a fulltext_search_state)"
(** Get fulltext_search state from search state. Raises [Web_app_error] if current state isn't an entry state. *)

let fulltext_search_state_to_sources_search_info {pattern; files; is_regex; is_case_sensitive; last_match_id} =
  let open Data_types in
  {
    pattern = pattern;
    files = files;
    is_regex = is_regex;
    is_case_sensitive = is_case_sensitive; 
    last_match_id = last_match_id;
  }
(** Converts [fulltext_search_state] to [Data_types.sources_search_info] *)

let state_to_info state =
  match state with
  | Uninitialized -> raise @@ web_app_error "state_to_info: couldn't get info from uninitialized search for fulltext search"
  | FulltextSearch state -> (fulltext_search_state_to_sources_search_info state)
(** Converts [search_state] to [Data_types.info].
    Raises [Web_app_error] if current state is uninitialised. *)

let get_input id = unopt @@ Html.CoerceTo.input @@ get_element_by_id id
(** Returns an input with given id *)

let show_it : sources_search_result_jsoo t -> unit =
  fun (result : sources_search_result_jsoo t) ->
  logs @@ string_of_int @@ result##.totaloccs
(** ok *)

let preview_fulltext_source pattern =
  let fulltext_info = {
    pattern;
    files = ML;
    is_regex = true;
    is_case_sensitive = true;
    last_match_id = 10;
  } in
  Lwt.async @@
  Requests.send_generic_request
    ~request:(Requests.getSources_fulltext @@ fulltext_search_state_to_sources_search_info @@ fulltext_info)
    ~callback:(fun sources_results ->
        (* begin
           match sources_results with
           | result ->
              show_it (Objects.sources_search_result_to_jsoo result);
           | _ -> raise @@ web_app_error "problem in preview sources fulltext"
           end; *)
        show_it (Objects.sources_search_result_to_jsoo sources_results);
        Lwt.return_unit
      )
    ~error:(fun err ->
        begin
          match err with
          | Unknown -> logs "something is wrong in preview_fulltext_source";
          | _ -> warn "Work is needed here";
        end;
        Lwt.return_unit
      )
(** Request to get sources fulltext result *)

(* let give_this_to_logs query_data =
   Requests.send_generic_request
    ~request:(Requests.getSources_fulltext query_data)
    ~callback:(fun sources_data ->
        show_it (Objects.sources_search_result_to_jsoo sources_data);
        Lwt.return_unit
      )
    () *)
(** ok *)

let set_handlers () =
  let fulltext_form = unopt @@ Html.CoerceTo.input @@ get_element_by_id "fpattern_fulltext" in

  fulltext_form##.onkeyup := Html.handler (fun kbevent ->
      let cur_input_value = fulltext_form##.value##trim in
      begin
        match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
        | Some "Space" -> logs "just pressed spacebar";    
        | _ -> preview_fulltext_source @@ (to_string cur_input_value);
        (* give_this_to_logs @@ state_to_info @@ !search_state; *)
      end;
      _false
    )
(** Trying to query api *)

let initialise_state () =
  let args = Url.Current.arguments in
  if args != []
  then search_state := state_of_args args
(** Initialises state by looking up current URL arguments (query string) *)

let uninitialized_page () =
  let forms = get_element_by_id "forms" in
  forms##.style##.display := js "";
  Lwt.return_unit
(** Displays unitialized version of the page. *)

let onload () =
  set_handlers ();
  initialise_state ();
  uninitialized_page ()
  (* match !search_state with
     | Uninitialized -> uninitialized_page ()
     | _ -> fulltext_page () *)
  (* Onload handler for fulltext search page *)

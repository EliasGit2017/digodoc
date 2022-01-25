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

module OrderedSource = struct
  type t = Data_types.file_type
  let compare e1 e2 =
    match e1, e2 with
    | x, y when x = y -> 0
    | ML, _ | DUNE, MAKEFILE -> -1
    | _ -> 1
end
(** Odered source type *)

module SourceSet = Set.Make(OrderedSource)
(** Set of sources *)

type fulltext_search_state = {
  mutable pattern : string;
  mutable sources : SourceSet.t;
  mutable current_source : Data_types.file_type;
  (* mutable files : Data_types.file_type; *)
  mutable is_regex : bool;
  mutable is_case_sensitive : bool;
}
(** State for fulltext search *)

type search_state = 
  | Uninitialized
  | FulltextSearch of fulltext_search_state
  (** Search state type within fulltext search page *)

let search_state = ref Uninitialized
(** Global variable that stores state of fulltext search page *)

let get_first_source = SourceSet.min_elt
(** Gets element from a set following order from below :
    - ML
    - DUNE
    - MAKEFILE *)

let state_of_args args = 
  match List.assoc "search" args with
  | "fulltext" ->
      let state = {
        pattern = "";
        sources = SourceSet.empty;
        current_source = ML;
        (* files = ML; need to find a way to take dune files and makefiles in consideration *)
        is_regex = true;
        is_case_sensitive = true;
      }
      in
      List.iter (fun (key, elt) ->
          match key with
          | "search" -> ()
          | "pattern" -> state.pattern <- decode_query_val elt
          | "sources" -> state.sources <- SourceSet.add (file_type_of_string elt) state.sources
          | "current" -> state.current_source <- file_type_of_string elt
          | "is_regex" -> (match elt with "text" -> state.is_regex <- false | _ -> state.is_regex <- true)
          | "is_case_sensitive" -> (match elt with "aA" -> state.is_case_sensitive <- true | _ -> state.is_case_sensitive <- false)
          | _ -> raise @@ web_app_error (Printf.sprintf "state_of_args: key %s is not recognised" key)
        )
        args;
      FulltextSearch state
  | s -> raise @@ web_app_error (Printf.sprintf "state_of_args: search type %s is not recognised in fulltext search" s)

let state_to_args state = 
  match state with 
  | Uninitialized -> ""
  | FulltextSearch {pattern; sources; current_source; is_regex; is_case_sensitive; page} ->
      Printf.sprintf "search=fulltext&pattern=%s&%s&current=%s&is_regex=%s&is_case_sensitive=%s&page=%d"
        (encode_query_val pattern)
        (String.concat "&"
         @@ List.map (fun elt -> ("files" ^ file_type_to_string elt))
         @@ SourceSet.elements sources)
        (file_type_to_string current_source)
        (if is_regex then "yes" else "text")
        (if is_case_sensitive then "aA" else "no")
        page

let state_to_args state = 
  Printf.sprintf "search=fulltext&pattern=%s&%s&current=%s&is_regex=%s&is_case_sensitive=%s"
    (encode_query_val state.pattern)
    (String.concat "&"
     @@ List.map (fun elt -> ("files" ^ file_type_to_string elt))
     @@ SourceSet.elements state.sources)
    (file_type_to_string state.current_source)
    (if state.is_regex then "yes" else "text")
    (if state.is_case_sensitive then "aA" else "no")
(** Comms *)

let get_sources_fulltext_state () =
  match !search_state with
  | FulltextSearch state -> state
  | _ -> raise @@ web_app_error "get_sources_fulltext_info : current state is not a source state"
(** Get fulltext_source state from search state. Raises [Web_app_error] if current state isn't a fulltext source state *)

let fulltext_search_state_to_fulltext_search_info {pattern; current_source; is_regex; is_case_sensitive; _} =
  let open Data_types in
  {
    pattern = pattern;
    files = current_source;
    is_regex = is_regex;
    last_match_id = 10;
    is_case_sensitive = is_case_sensitive; 
  }
(** Converts [fulltext_search_state] to [Data_types.sources_search_info] *)

let state_to_info state =
  match state with
  | Uninitialized -> raise @@ web_app_error "state_to_info: couldn't get info from uninitialized search for fulltext search"
  | FulltextSearch state -> Sources_Fulltext (fulltext_search_state_to_fulltext_search_info state) *)
(** Converts [search_state] to [Data_types.info].
    Raises [Web_app_error] if current state is uninitialised. *)

    let get_input id = unopt @@ Html.CoerceTo.input @@ get_element_by_id id
    (** Returns an input with given id *)

    let update_sources_fulltext_state () =
    let fulltext_search_state = {
    pattern = "";
    sources = SourceSet.empty;
    current_source = ML;
    (* files = ML; need to find a way to take dune files and makefiles in consideration *)
    is_regex = true;
    is_case_sensitive = true;
    }
    in    
    let pattern_input = get_input "fpattern_fulltext" in
    let value = to_string pattern_input##.value##trim in
    fulltext_search_state.pattern <- value;
    fulltext_search_state.is_regex <- to_bool (get_input "fregex")##.checked;
    fulltext_search_state.is_case_sensitive <- to_bool (get_input "fcase_sens")##.checked;
    match fulltext_search_state.sources with
    | set when SourceSet.is_empty set -> false
    | set ->
    fulltext_search_state.current_source <- get_first_source set;
    search_state := fulltext_search_state;
    true
    (** comment this *)

    let update_form () =
    let check_input id =
    (get_input id)##.checked := bool true
    in
    match !search_state with
    | Uninitialized ->
    raise @@ web_app_error "update_form: search is unitialised"
    | FulltextSearch state ->
    (get_input "fpattern_fulltext")##.value := js state.pattern;
    SourceSet.iter (fun source -> check_input @@ "f" ^ file_type_to_string source)
     state.sources;
    if state.is_regex then check_input "fregex";
    if state.is_case_sensitive then check_input "fcase_sens"
    (** update *)

    let preview_Sources pattern files =
    let sources_search_info = {
    pattern;
    files;
    is_regex = true;
    is_case_sensitive = true;
    last_match_id = 10; 
    } in
    Lwt.async @@
    Requests.send_generic_request
    ~request:(Requests.getSources_fulltext sources_search_info)
    ~callback:(fun source_search_result ->
     if not @@ (source_search_result.occs = [])
     then
       begin
         Insertion.insert_Sources_fulltext (Objects.sources_search_result_to_jsoo source_search_result);
       end;  
     Lwt.return_unit
    )
    ~error:(fun err ->
     begin
       match err with
       | Unknown ->
           logs "Something went wrong in preview_Sources"
       | _ ->
           warn "Work on preview_Sources";
     end;
     Lwt.return_unit
    )
    (** Request to get sources for fulltext search *)

    let set_handlers () = ()

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

    let fulltext_page () =
    let get_current state =
    match state with
    | Uninitialized -> raise @@ web_app_error "get_elt_from_state: search state is unitialised"
    | FulltextSearch state -> file_type_to_string state.current_source
    and set_current state current =
    match state with
    | Uninitialized -> raise @@ web_app_error "set_current: search state is uninitialised"
    | FulltextSearch state -> FulltextSearch {state with current_source = file_type_of_string current}
    and get_elts_from_state state =
    match state with
    | Uninitialized -> raise @@ web_app_error "get_elts_from_state: search state is uninitialised"
    | FulltextSearch state -> List.map file_type_to_string @@ SourceSet.elements state.sources
    and link_to_elt state link =
    let st =
    match state with
    | Uninitialized -> raise @@ web_app_error "link_to_elt: search state is uninitialised"
    | FulltextSearch state -> FulltextSearch {state with page = 1}
    in
    let href = "fulltext_search.html?" ^ state_to_args st in
    link##setAttribute (js "href") (js href)
    in

    (** Constructs and displays fulltext_search page *)

    let onload () =
    set_handlers ();
    initialise_state ();
    match !search_state with
    | Uninitialized -> uninitialized_page ()
    | _ -> fulltext_page ()
    (* Onload handler for fulltext search page *)

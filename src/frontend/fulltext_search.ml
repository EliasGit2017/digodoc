(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro SAS & Origin Labs SAS                     *)
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


type fulltext_search_state = {
  mutable pattern : string;
  mutable files : file_type;
  mutable is_regex : bool;
  mutable is_case_sensitive : bool;
  mutable last_match_id : int
}
(** State for fulltext search *)

let state = {
  pattern = "";
  files = ML;
  is_regex = true;
  is_case_sensitive = true;
  last_match_id = 0
}
(** Default search_state updated through deref *)

let search_state = ref state
(** Global variable that stores state of fulltext search page *)

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

let get_input id = unopt @@ Html.CoerceTo.input @@ get_element_by_id id
(** Returns an input with given id *)

let set_attr elt attr value =
  elt##setAttribute (js attr) value
(** [set_attr elt attr val] sets attribute [attr] of [elt] to [value]. *)

let append_inner elt str =
  elt##.innerHTML := concat elt##.innerHTML str
(** [append_inner elt str] appends [str] to the content of [elt]. *)

let preview_fulltext_source pattern regex case_sens loadmore =
  let handle_checkbox id state =
    let target =
      match id with
      | "fcase_ftype_ml" -> ML
      | "fcase_ftype_dune" -> DUNE
      | "fcase_ftype_makefile" -> MAKEFILE
      | _ -> raise @@ web_app_error "Error in preview_fulltext_source -> handle_checkbox"
    in
    if to_bool @@ (get_input id)##.checked
    then state.files <- target
  in
  (* Init fulltext_info *)
  let fulltext_info = {
    pattern;
    files = ML;
    is_regex = regex;
    is_case_sensitive = case_sens;
    last_match_id = !search_state.last_match_id;
  } in
  handle_checkbox "fcase_ftype_ml" fulltext_info;
  handle_checkbox "fcase_ftype_dune" fulltext_info;
  handle_checkbox "fcase_ftype_makefile" fulltext_info;
  Lwt.async @@
  Requests.send_generic_request
    ~request:(Requests.getSources_fulltext @@ fulltext_search_state_to_sources_search_info @@ fulltext_info)
    ~callback:(fun sources_results ->
        Insertion.insert_Fulltext_Sources (Objects.sources_search_result_to_jsoo sources_results) loadmore;
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

let set_handlers () =
  let fulltext_form = unopt @@ Html.CoerceTo.input @@ get_element_by_id "fpattern_fulltext" in
  let ml_switch = get_input "fcase_ftype_ml" in
  let dune_switch = get_input "fcase_ftype_dune" in
  let mkfile_switch = get_input "fcase_ftype_makefile" in
  let load_more_btn = unopt @@ Html.CoerceTo.button @@ get_element_by_id "load_more" in

  ml_switch##.onchange := Html.handler ( fun _ ->
      let cur_input_value = fulltext_form##.value##trim in
      let is_regex = to_bool @@ (get_input "fregex")##.checked in
      let case_sens = to_bool @@ (get_input "fcase_sens")##.checked in
      state.last_match_id <- 0;
      if to_bool ml_switch##.checked
      then
        begin
          dune_switch##.checked := _false;
          mkfile_switch##.checked := _false;
          preview_fulltext_source (to_string cur_input_value) is_regex case_sens false
        end
      else
        begin
          (* Must select one type of files to perform fulltext search, ML by default *)
          if ((not @@ to_bool dune_switch##.checked) && (not @@ to_bool mkfile_switch##.checked))
          then
            begin
              ml_switch##.checked := _true;
              preview_fulltext_source (to_string cur_input_value) is_regex case_sens false
            end
        end;
      _false
    );
  (** Only one switch at a time *)

  dune_switch##.onchange := Html.handler ( fun _ ->
      let cur_input_value = fulltext_form##.value##trim in
      let is_regex = to_bool @@ (get_input "fregex")##.checked in
      let case_sens = to_bool @@ (get_input "fcase_sens")##.checked in
      state.last_match_id <- 0;
      if to_bool dune_switch##.checked
      then
        begin
          ml_switch##.checked := _false;
          mkfile_switch##.checked := _false;
          preview_fulltext_source (to_string cur_input_value) is_regex case_sens false
        end
      else
        begin
          (* Must select one type of files to perform fulltext search, ML by default *)
          if ((not @@ to_bool ml_switch##.checked) && (not @@ to_bool mkfile_switch##.checked))
          then
            begin
              ml_switch##.checked := _true;
              preview_fulltext_source (to_string cur_input_value) is_regex case_sens false
            end
        end;
      _false
    );
  (** Only one switch at a time *)

  mkfile_switch##.onchange := Html.handler ( fun _ ->
      let cur_input_value = fulltext_form##.value##trim in
      let is_regex = to_bool @@ (get_input "fregex")##.checked in
      let case_sens = to_bool @@ (get_input "fcase_sens")##.checked in
      state.last_match_id <- 0;
      if to_bool mkfile_switch##.checked
      then
        begin
          ml_switch##.checked := _false;
          dune_switch##.checked := _false;
          preview_fulltext_source (to_string cur_input_value) is_regex case_sens false
        end
      else
        begin
          (* Must select one type of files to perform fulltext search, ML by default *)
          if ((not @@ to_bool dune_switch##.checked) && (not @@ to_bool ml_switch##.checked))
          then
            begin
              ml_switch##.checked := _true;
              preview_fulltext_source (to_string cur_input_value) is_regex case_sens false
            end
        end;
      _false
    );
  (** Only one switch at a time *)

  fulltext_form##.onkeyup := Html.handler (fun kbevent ->
      let cur_input_value = fulltext_form##.value##trim in
      let is_regex = to_bool @@ (get_input "fregex")##.checked in
      let case_sens = to_bool @@ (get_input "fcase_sens")##.checked in
      state.last_match_id <- 0;
      begin
        match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
        | Some "Space" -> 
            logs "user just pressed spacebar";
            preview_fulltext_source (to_string cur_input_value) is_regex case_sens false;   
        | _ -> preview_fulltext_source (to_string cur_input_value) is_regex case_sens false;
      end;
      load_more_btn##.style##.display := js "block";
      _false
    );
  (** Query search-api and display result 20 by 20 *)

  fulltext_form##.onpointerenter := Html.handler (fun _ ->
      let time = 800. in
      let regex_inst = unopt @@ Html.CoerceTo.div @@ get_element_by_id "regex_instructions" in
      regex_inst##.style##.opacity := (js "0") |> Js.Optdef.return;
      regex_inst##.style##.display := js "block";
      let last = ref Js.date##now in

      let rec tick () =
        let updated_opacity = (js (string_of_float ((float_of_string (to_string (unoptdef @@ regex_inst##.style##.opacity))) +. ((Js.date##now -. !last) /. time)))) in
        regex_inst##.style##.opacity := updated_opacity |> Js.Optdef.return;
        last := Js.date##now;
        if ((float_of_string (to_string updated_opacity)) < 1.)
        then window##requestAnimationFrame(Js.wrap_callback (fun _ -> tick ())) |> ignore;
      in

      tick ();
      _false
    );
  (** Shows regex instructions when pointer is over the text entry form [fulltext-form] (proceeds by slowly increasing opacity
      for [time] ms after div [regex_instructions]'s display style option is set to block) *)

  fulltext_form##.onpointerleave := Html.handler (fun _ ->
      let time = 800. in
      let regex_inst = unopt @@ Html.CoerceTo.div @@ get_element_by_id "regex_instructions" in
      regex_inst##.style##.opacity := (js "1") |> Js.Optdef.return;
      let last = ref Js.date##now in

      let rec tick () =
        let updated_opacity = (js (string_of_float ((float_of_string (to_string (unoptdef @@ regex_inst##.style##.opacity))) -. ((Js.date##now -. !last) /. time)))) in
        regex_inst##.style##.opacity := updated_opacity |> Js.Optdef.return;
        last := Js.date##now;
        if ((float_of_string (to_string updated_opacity)) > 0.)
        then window##requestAnimationFrame(Js.wrap_callback (fun _ -> tick ())) |> ignore
        else
          begin
            regex_inst##.style##.display := js "none";
            Headfoot.footerHandler();
          end
      in

      tick ();
      _false
    );
  (** Hides regex instructions when pointer leaves the text entry form [fulltext-form] (proceeds by slowly decreasing opacity
      for [time] ms and sets div [regex_instructions]'s display style option to none when opacity gets to 0) *)

  load_more_btn##.onclick := Html.handler (fun _ ->
      let cur_input_value = fulltext_form##.value##trim in
      let is_regex = to_bool @@ (get_input "fregex")##.checked in
      let case_sens = to_bool @@ (get_input "fcase_sens")##.checked in
      let current_last = !search_state.last_match_id in
      state.last_match_id <- current_last + 20;
      logs @@ string_of_int @@ current_last;
      preview_fulltext_source (to_string cur_input_value) is_regex case_sens true;
      _false
    )
(** Loads at most the 20 next results if available *)

let uninitialized_page () =
  let forms = get_element_by_id "forms" in
  forms##.style##.display := js "";
  Lwt.return_unit
(** Displays unitialized version of the page. *)

let onload () =
  set_handlers ();
  uninitialized_page ()
(* Onload handler for fulltext search page *)

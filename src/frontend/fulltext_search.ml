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
open Utils

open Objects


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

let set_attr elt attr value =
  elt##setAttribute (js attr) value
(** [set_attr elt attr val] sets attribute [attr] of [elt] to [value]. *)

let get_attr elt attr =
  elt##getAttribute (js attr)
(** [get_attr elt attr] retrieves attribute [attr] of [elt] *)

let append_inner elt str =
  elt##.innerHTML := concat elt##.innerHTML str
(** [append_inner elt str] appends [str] to the content of [elt]. *)

let insert_Fulltext_Sources : sources_search_result_jsoo t -> unit =
  fun (result : sources_search_result_jsoo t) ->
  let current_pattern = unopt @@ Html.CoerceTo.input @@ get_element_by_id "fpattern_fulltext" in
  let result_div = unopt @@ Html.CoerceTo.div @@ get_element_by_id "result-div" in
  let res_ol = unopt @@ Html.CoerceTo.ol @@ get_element_by_id "results-list" in
  let page_info = unopt @@ Html.CoerceTo.div @@ get_element_by_id "page-info" in
  let occurences_text = Html.createP document in

  page_info##.innerHTML := js "";
  res_ol##.innerHTML := js "";

  if not @@ ((to_string current_pattern##.value##trim) = "")
  then
    begin
      result_div##.style##.display := js "block";
      occurences_text##.innerHTML := js (Printf.sprintf "<b>%d</b> results found for <b>%s<b>" result##.totaloccs (to_string current_pattern##.value##trim));
      occurences_text##.style##.textAlign := js "center";
      Dom.appendChild page_info occurences_text;

      foreach
        (fun _ elt ->
           let source_occ_ul = Html.createUl document in
           (* let occ_position = Html.createA document in *)
           let occ_line = Html.createCode document in
           let line1 = Html.createLi document in
           let line2 = Html.createLi document in
           let line2_div = Html.createDiv document in
           let line2_a = Html.createA document in
           let line2_a_div = Html.createDiv document in
           let line2_a_div_tab = Html.createTable document in
           let line2_a_div_tab_body = Html.createTbody document in
           let line2_a_div_tab_tr = Html.createTr document in
           let line2_a_div_tab_tr_td1 = Html.createTd document in
           let line2_a_div_tab_tr_td2 = Html.createTd document in
           let opam_name_span = Html.createA document in
           let opam_ns_href = concat (concat path_to_root  elt##.srcpath) (js "/index.html") in
           let occu_path_href = (concat path_to_root elt##.occpath) in
           let filename = Html.createSpan document in

           set_attr source_occ_ul "class" (js "fulltext-ul");
           set_attr opam_name_span "class" (js "opam-name");
           set_attr opam_name_span "href" opam_ns_href;
           append_inner line1 (js " In ");
           append_inner opam_name_span elt##.opamname;

           Dom.appendChild line1 opam_name_span;

           append_inner line1 (js " in ");
           set_attr filename "class" (js "f_filename");
           append_inner filename elt##.filename;

           Dom.appendChild line1 filename;
           Dom.appendChild source_occ_ul line1;

           set_attr line2_div "class" (js "link-to-docs-sources");
           (* line2_div##.style##.marginTop := js "7px"; *)
           set_attr line2_a_div_tab_tr_td1 "class" (js "occ-position");
           append_inner line2_a_div_tab_tr_td1 (js (string_of_int elt##.occpos));

           set_attr line2_a_div_tab_tr_td2 "class" (js "no_underline");
           append_inner occ_line elt##.occline;
           Dom.appendChild line2_a_div_tab_tr_td2 occ_line;

           append_inner line2_a_div_tab_tr (js "At line ");
           line2_a_div_tab_tr##.style##.marginLeft := js "2%";

           Dom.appendChild line2_a_div_tab_tr line2_a_div_tab_tr_td1;
           append_inner line2_a_div_tab_tr (js " ");
           Dom.appendChild line2_a_div_tab_tr line2_a_div_tab_tr_td2;


           Dom.appendChild line2_a_div_tab_body line2_a_div_tab_tr;
           Dom.appendChild line2_a_div_tab line2_a_div_tab_body;
           Dom.appendChild line2_a_div line2_a_div_tab;

           set_attr line2_a "href" occu_path_href;
           set_attr line2_a "class" (js "no_underline no_underline.wide");

           line2_a_div##.style##.marginTop := js "0%";

           Dom.appendChild line2_a line2_a_div;
           Dom.appendChild line2_div line2_a;
           Dom.appendChild line2 line2_div;

           Dom.appendChild source_occ_ul line2;
           Dom.appendChild res_ol source_occ_ul;            
        )
        result##.occs
    end
  else result_div##.style##.display := js "none";
  Headfoot.footerHandler ();
  logs @@ string_of_int @@ result##.totaloccs
(** ok *)

let preview_fulltext_source pattern regex case_sens =
  let fulltext_info = {
    pattern;
    files = ML;
    is_regex = regex;
    is_case_sensitive = case_sens;
    last_match_id = 0;
  } in
  Lwt.async @@
  Requests.send_generic_request
    ~request:(Requests.getSources_fulltext @@ fulltext_search_state_to_sources_search_info @@ fulltext_info)
    ~callback:(fun sources_results ->
        insert_Fulltext_Sources (Objects.sources_search_result_to_jsoo sources_results);
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

  fulltext_form##.onkeyup := Html.handler (fun kbevent ->
      let cur_input_value = fulltext_form##.value##trim in
      let is_regex = to_bool @@ (get_input "fregex")##.checked in
      let case_sens = to_bool @@ (get_input "fcase_sens")##.checked in
      begin
        match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
        | Some "Space" -> 
            logs "just pressed spacebar";
            preview_fulltext_source (to_string cur_input_value) is_regex case_sens;   
        | _ -> preview_fulltext_source (to_string cur_input_value) is_regex case_sens;
      end;
      _false
    );
  (** Query search-api and display result 20 by 20 *)

  (** The two following functions are bad ... really playing on my nerves as requestAnimationFrame is not taken in consideration *)
  fulltext_form##.onpointerenter := Html.handler (fun _ ->
      let time = 800. in
      let regex_inst = unopt @@ Html.CoerceTo.div @@ get_element_by_id "regex_instructions" in
      regex_inst##.style##.opacity := (js "0") |> Js.Optdef.return;
      set_attr regex_inst "opacity" (js "0");
      regex_inst##.style##.display := js "block";
      let last = ref Js.date##now in

      let rec tick () =
        begin
          let tmp = (js (string_of_float ((float_of_string (to_string (unopt @@ (get_attr regex_inst "opacity")))) +. ((Js.date##now -. !last) /. time)))) in
          regex_inst##.style##.opacity := tmp |> Js.Optdef.return;
          set_attr regex_inst "opacity" tmp;
          last := Js.date##now;
          if (float_of_string (to_string (unopt @@ (get_attr regex_inst "opacity"))) < 1.)
          then
            begin
              logs "opacity < 1.";
              window##requestAnimationFrame(Js.wrap_callback (fun _ -> tick ())) |> ignore;
            end
            (* else regex_inst##.style##.display := js "block" *)
        end
      in

      tick (); 
      logs "mouse over regex_instructions";
      _false
    );
  (** Mouse over text entry animation *)


  fulltext_form##.onpointerleave := Html.handler (fun _ ->
      let time = 800. in
      let regex_inst = unopt @@ Html.CoerceTo.div @@ get_element_by_id "regex_instructions" in
      set_attr regex_inst "opacity" (js "1");
      let last = ref Js.date##now in

      let rec tick _ =
        begin
          let tmp = (js (string_of_float ((float_of_string (to_string (unopt @@ (get_attr regex_inst "opacity")))) -. ((Js.date##now -. !last) /. time)))) in
          regex_inst##.style##.opacity := tmp |> Js.Optdef.return;
          set_attr regex_inst "opacity" tmp;
          last := Js.date##now;
          if (float_of_string (to_string (unopt @@ (get_attr regex_inst "opacity"))) > 0.)
          then
            begin
              logs "opacity < 1.";
              window##requestAnimationFrame(Js.wrap_callback tick) |> ignore;
            end
          else regex_inst##.style##.display := js "none"
        end
      in
      window##requestAnimationFrame(Js.wrap_callback tick) |> ignore;
      logs "mouse quitting regex_instructions";
      _false
    )
(** Mouseout of text entry *)


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

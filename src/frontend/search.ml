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

(** Module [Search] defines behaviour for pages that has search input that displays results 
    in the generated from below list (all pages except index pages). Input lists only 10 packages, 
    libraries and modules starting from the third charachter entered by user. Click on search result
    sends to corresponding documentation page. Click on button leftside on the left of input (as well 
    pressing Enter button) redirect either to the search page with all results either to the documentation 
    page if there is only one result. *)

let redirection_handler () =
    (* Get the list of search items (results) *)
    let search_items = document##getElementsByClassName (js "search-item") in
    if search_items##.length == 1
    then 
        (* If only one result exists than redirect to the documentation page *)
        let item =  unopt @@ search_items##item 0 in
        let link =  unopt @@ Dom.CoerceTo.element @@ unopt @@ item##.firstChild in
        let href = unopt @@ link##getAttribute (js "href") in
        open_url href
    else
        (* Else redirect to search page with predefined filters *)
        let display_query = "&current=packages&page=1" in
        let entries_query = "&entry=packages&entry=libraries&entry=modules" in
        let url = concat path_to_root @@ 
            js ("search.html?search=entry&pattern=" 
                ^ encode_query_val entry_state.pattern 
                ^ entries_query 
                ^ display_query) in 
        open_url url
(** Redirection handler that redirects either to the documentation page (if only one result displayed) either
    to the search page that lists all the results. *)

let local_redirection_handler pattern = 
    (* TODO : modules *) 
    let display_query = "&current=vals&page=1" in
    (* TODO : add more elements *)
    let entries_query = "&element=vals" in
    let opam_cond = 
        match opam_name_from_page with
        | Some s -> "&opam=" ^ s
        | _ -> ""
    in 
    let url = concat path_to_root @@ 
        js ("search.html?search=element&pattern=" 
            ^ encode_query_val pattern 
            ^ entries_query 
            ^ display_query
            ^ opam_cond) in 
    open_url url
(* TODO : doc *)

let initialise () =
    let search_div = Html.createDiv document in
    search_div##.id := js "search-result";
    let ul = Html.createUl document in
    ul##.id := js "search-result-ul";
    Dom.appendChild search_div ul;
    append_page search_div;
    Lwt.return_unit
(** Creates a zone on the page where search results will be displayed *)

let clear_search () = 
    let ul = get_element_by_id "search-result-ul" in
    ul##.innerHTML := js ""
(** Removes all results from list *)

let update_search pattern =
    (* Sends request to get and display search results *)
    Lwt.async @@ 
        Requests.send_generic_request
            ~request:(Requests.search pattern)
            ~callback:(fun search_result ->
                (* display search results *)
                valid_input "search";
                Insertion.insert_search_result (Objects.search_result_to_jsoo search_result); 
                Lwt.return_unit
            )
            ~error:(fun err ->
                (* if occured error is Invalid_regex - color input to red *) 
                begin 
                    match err with
                    | Invalid_regex -> invalid_input "search"
                    | _ -> warn "Unknown error"
                end;
                Lwt.return_unit
            )
(** Sends request to get new search results and to update the zone that displays them *)

let set_search_handler () =
    let search = unopt @@ Html.CoerceTo.input @@ get_element_by_id "search" in
    let local_search_opt = get_element_by_id_opt "localsearch" in
    (* Handler called when keyup event is generated by search input *)
    search##.onkeyup := Html.handler (fun kbevent ->
        begin
            match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
            | Some "Enter" -> 
                (* if key is 'Enter' then redirect to the search page *)
                redirection_handler ()
            | Some "Escape" ->
                clear_search ();
            | _ ->
                (* clear search results *)
                clear_search ();
                (* state update *)
                let re = search##.value in
                let input = re##trim in
                entry_state.pattern <- to_string input;
                (* update search if pattern has more than 2 letters *)
                if String.length entry_state.pattern > 2
                then update_search entry_state.pattern;
        end;
        _false);
    (* Handler called when keyup event is generated by local search input (if present) *)
    match Opt.to_option local_search_opt with
    | Some x ->
        let local_search = unopt @@ Html.CoerceTo.input x in
        local_search##.onkeyup := Html.handler (fun kbevent ->
            begin
                match Option.map to_string @@ Optdef.to_option @@ kbevent##.key with
                | Some "Enter" ->
                    let re = local_search##.value in
                    let input = to_string @@ re##trim in
                    local_redirection_handler input
                | _ -> clear_search ();
            end;
            _false)
    | None -> ()
(** Sets onkeyup handlers that will be called when user enters key in search inputs. If key is 'Enter' 
    then [redirection_handler] or [local_redirection_handler] is called. *)

let set_button_handler () =
    let button = unopt @@ Html.CoerceTo.button @@ get_element_by_id "search-button" in
    button##.onclick := Html.handler (fun _ ->
        (* if button is pressed then call redirection handler *)
        redirection_handler ();
        _false)
(** Sets onclick handler that will be called when user click on the search button. *)

let onload () =
    (* set search input handler *)
    set_search_handler ();
    (* set button handler *)
    set_button_handler ();
    (* initiate page *)
    initialise ()

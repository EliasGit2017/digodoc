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

let insert_Sources_fulltext : sources_search_result_jsoo t -> unit = 
  fun (sources : sources_search_result_jsoo t) ->
  sources
(** Insert Sources results for fulltext search *)

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
            insert_Sources_fulltext (Objects.sources_search_result_to_jsoo source_search_result);
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
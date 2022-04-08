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
open Objects
open Globals
open Data_types


module StringSet = Set.Make(String)

(** Module [Insertion] unions functions that make various DOM insertions. *)

let set_attr elt attr value =
  elt##setAttribute (js attr) value
(** [set_attr elt attr val] sets attribute [attr] of [elt] to [value]. *)    

let append_inner elt str =
  elt##.innerHTML := concat elt##.innerHTML str
(** [append_inner elt str] appends [str] to the content of [elt]. *)

(** {1 Index page} *)

let display_header head = 
  let name_elt : Html.element t = 
    get_element_by_id @@ "name-" ^ to_string head in
  name_elt##.style##.display := js ""
(** [display_header head] displays header [head] within index page. [head] represents first letter of entries
    that he unites. *)

let get_first_letter elt =
  (elt##.name##charAt 0)##toLowerCase
(** [get_first_letter elt] returns the first letter of an entry/element [elt]. *)

let write_message message =
  let msg_div = Html.createDiv document in
  let msg = Html.createSpan document in
  set_attr msg "class" (js "message");
  append_inner msg (js message);
  Dom.appendChild msg_div msg;
  append_content msg_div
(** Displays message at the bottom of the page. *)

let write_message_id message id1 id2 =
  let msg_div = unopt @@ Html.CoerceTo.div @@ get_element_by_id id1 in
  let msg = unopt @@ Html.CoerceTo.element @@ get_element_by_id id2 in
  msg##.innerText := js "";
  msg##.innerText := js message;
  msg_div##.style##.display := js "block";
  Headfoot.footerHandler()
(** Displays message at the bottom of the page. *)

let write_warning warn =
  (* Create structure *)
  let warning_div = Html.createDiv document 
  and table = Html.createTable document 
  and tr = Html.createTr document 
  and td_image = Html.createTd document 
  and td_mess = Html.createTd document  in
  let img_src = concat path_to_root (js "static/imgs/warning.png") in
  let img = Html.createImg document in
  set_attr img "src" img_src;
  set_attr td_image "class" (js "warning-img");
  Dom.appendChild td_image img;
  let mess = Html.createSpan document in
  set_attr td_mess "class" (js "warning-msg");
  append_inner mess (js warn);
  set_attr warning_div "class" (js "warning");
  Dom.appendChild td_mess mess;
  Dom.appendChild tr td_image;
  Dom.appendChild tr td_mess;
  Dom.appendChild table tr;
  Dom.appendChild warning_div table;
  append_content warning_div
(** Displays warning at the bottom of the page. *)

let insert_packages_index : packages_jsoo t -> unit  = 
  fun (packages : packages_jsoo t) ->
  let first_letter : js_string t ref = ref (js "") in
  (* for every package *)
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let pkg = Html.createLi document in
       set_attr pkg "class" (js "package");
       (* Append opam package name *)
       let pkg_name = Html.createA document in
       set_attr pkg_name "class" (js "digodoc-opam");
       set_attr pkg_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.name;
       Dom.appendChild pkg_name name;
       Dom.appendChild pkg pkg_name;
       (* Append synopsis *)
       append_inner pkg @@ concat (js " ") elt##.synopsis;
       (* Displaying header of package if it's not already done *)
       let elt_first_letter = get_first_letter elt in
       if not (!first_letter = elt_first_letter) then begin
         first_letter := elt_first_letter;
         display_header elt_first_letter;
       end;
       let pkg_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
       Dom.appendChild pkg_set pkg;
    )
    packages
(** [insert_packages_index pkgs] constructs index page and inserts array of packages [pkgs] that stores 
    converted to js package objects. *)

let insert_libraries_index : libraries_jsoo t -> unit  = 
  fun (libraries : libraries_jsoo t) -> 
  let first_letter : js_string t ref = ref (js "") in
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let lib = Html.createLi document in
       set_attr lib "class" (js "package");
       (* Append library name *)
       let lib_name = Html.createA document in
       set_attr lib_name "class" (js "digodoc-lib");
       set_attr lib_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.name;
       Dom.appendChild lib_name name;
       Dom.appendChild lib lib_name;
       (* Append library package *)
       append_inner lib (js " in opam ");
       let lib_opam = Html.createA document in
       set_attr lib_opam "class" (js "digodoc-opam");
       set_attr lib_opam "href" elt##.opampath;
       append_inner lib_opam elt##.opam;
       Dom.appendChild lib lib_opam;
       let elt_first_letter = get_first_letter elt in
       if not (!first_letter = elt_first_letter) then begin
         first_letter := elt_first_letter;
         display_header elt_first_letter;                
       end;
       let lib_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
       Dom.appendChild lib_set lib;
    )
    libraries
(** Same as [insert_packages_index] but instead deals with libraries. *)

let insert_modules_index : modules_jsoo t -> unit  = 
  fun (modules : modules_jsoo t) -> 
  let first_letter : js_string t ref = ref (js "") in
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let mdl = Html.createLi document in
       set_attr mdl "class" (js "package");
       (* Append module name *)
       let mdl_name = Html.createA document in
       set_attr mdl_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.name;
       Dom.appendChild mdl_name name;
       Dom.appendChild mdl mdl_name;
       (* Append module package *)
       append_inner mdl (js " in opam ");
       let mdl_opam = Html.createA document in
       set_attr mdl_opam "class" (js "digodoc-opam");
       set_attr mdl_opam "href" elt##.opampath;
       append_inner mdl_opam elt##.opam;
       Dom.appendChild mdl mdl_opam;
       (* Append module libraries *)                
       if elt##.libs##.length > 0 then append_inner mdl (js " in libs ");
       (* for every library *)
       foreach
         (fun i lib -> 
            let mdl_libs = Html.createA document in
            set_attr mdl_libs "class" (js "digodoc-lib");
            set_attr mdl_libs "href" lib##._1;
            append_inner mdl_libs lib##._0;
            Dom.appendChild mdl mdl_libs;
            if i+1 < elt##.libs##.length then append_inner mdl (js ", ")
         )
         elt##.libs;
       let elt_first_letter = get_first_letter elt in
       if not (!first_letter = elt_first_letter) then begin
         first_letter := elt_first_letter;
         display_header elt_first_letter;                
       end;
       let mdl_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
       Dom.appendChild mdl_set mdl;
    )
    modules
(** Same as [insert_packages_index] but instead deals with modules. *)

let insert_metas_index : metas_jsoo t -> unit  = 
  fun (metas : metas_jsoo t) -> 
  let first_letter : js_string t ref = ref (js "") in
  foreach 
    (fun _ elt ->
       (* Create a line *)
       let meta = Html.createLi document in
       set_attr meta "class" (js "package");
       (* Append meta name *)
       let meta_name = Html.createA document in
       set_attr meta_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.namemeta;
       Dom.appendChild meta_name name;
       Dom.appendChild meta meta_name;
       (* Append meta package *)
       append_inner meta (js " in opam ");
       let meta_opam = Html.createA document in
       set_attr meta_opam "class" (js "digodoc-opam");
       set_attr meta_opam "href" elt##.opampath;
       append_inner meta_opam elt##.opam;
       Dom.appendChild meta meta_opam;
       let elt_first_letter = (elt##.namemeta##charAt 0)##toLowerCase in
       if not (!first_letter = elt_first_letter) then begin
         first_letter := elt_first_letter;
         display_header elt_first_letter;                
       end;
       let meta_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
       Dom.appendChild meta_set meta
    )
    metas
(** Same as [insert_packages_index] but instead deals with metas. *)

let insert_sources_index : sources_jsoo t -> unit  = 
  fun (sources : sources_jsoo t) -> 
  let first_letter : js_string t ref = ref (js "") in
  foreach 
    (fun _ elt ->
       (* Create a line *)
       let src = Html.createLi document in
       set_attr src "class" (js "package");
       (* Append source name *)
       let src_name = Html.createA document in
       set_attr src_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.namesrc;
       Dom.appendChild src_name name;
       Dom.appendChild src src_name;
       (* Append source package *)
       append_inner src (js " in opam ");
       let src_opam = Html.createA document in
       set_attr src_opam "class" (js "digodoc-opam");
       set_attr src_opam "href" elt##.opampath;
       append_inner src_opam elt##.opam;
       Dom.appendChild src src_opam;
       let elt_first_letter = (elt##.namesrc##charAt 0)##toLowerCase in
       if not (!first_letter = elt_first_letter) then begin
         first_letter := elt_first_letter;
         display_header elt_first_letter;                
       end;
       let src_set = get_element_by_id @@ "packages-" ^ to_string !first_letter in
       Dom.appendChild src_set src;
    )
    sources
(** Same as [insert_packages_index] but instead deals with sources. *)

let insert_index entries =
  match entries with
  | Opam packages -> insert_packages_index (Objects.packages_to_jsoo packages)
  | Lib libraries -> insert_libraries_index (Objects.libraries_to_jsoo libraries)
  | Mdl modules -> insert_modules_index (Objects.modules_to_jsoo modules)
  | Meta metas -> insert_metas_index (Objects.metas_to_jsoo metas)
  | Src sources -> insert_sources_index (Objects.sources_to_jsoo sources)
(** Calls specific to [entries] insertion function for index page *)

(** {1 Search input} *)

let insert_search_result : search_result_jsoo t -> unit =
  fun (result : search_result_jsoo t) ->
  (* Get ul element where search results will be inserted *)
  let search_ul = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "search-result-ul" in
  (* Insert package in search ul *)
  let insert_pack pack =
    (* Create a package line *)
    let search_item = Html.createLi document in
    set_attr search_item "class" (js "search-item");
    let item_link = Html.createA document in
    set_attr item_link "href" @@ concat path_to_root pack##.path;
    Dom.appendChild search_item item_link;
    let item_indicator = Html.createDiv document
    and item_name = Html.createDiv document in
    set_attr item_indicator "class" (js ("item-indicator item-pack"));
    set_attr item_name "class" (js "item-name");
    append_inner item_name pack##.name;
    Dom.appendChild item_link item_indicator;
    Dom.appendChild item_link item_name;
    Dom.appendChild search_ul search_item
  (* Insert module/library in search ul *)
  and insert_item elt indicator =
    (* Create an entry line *)
    let search_item = Html.createLi document in
    set_attr search_item "class" (js "search-item");
    let item_link = Html.createA document in
    set_attr item_link "href" @@ concat path_to_root elt##.path;
    Dom.appendChild search_item item_link;
    let item_indicator = Html.createDiv document
    and item_name = Html.createDiv document in
    set_attr item_indicator "class" (js ("item-indicator " ^ indicator));
    set_attr item_name "class" (js "item-name");
    append_inner item_name elt##.name;
    (* Show entry's package *)
    let item_pack = Html.createDiv document in
    set_attr item_pack "class" (js "package-item");
    append_inner item_pack elt##.opam;
    Dom.appendChild item_link item_indicator;
    Dom.appendChild item_link item_name;
    Dom.appendChild item_link item_pack;
    Dom.appendChild search_ul search_item
  in 
  (* For every package in search result *)
  foreach
    (fun _ elt -> insert_pack elt)
    result##.packages;
  (* For every library in search result *)
  foreach
    (fun _ elt -> insert_item elt "item-lib")
    result##.libraries;
  (* For every module in search result *)
  foreach
    (fun _ elt -> insert_item elt "item-mdl")
    result##.modules
(** [insert_search_result search_res] fills search list with entries in [res]. 
    [search_res] should be converted to js object from [Data_types.search_result] type. *)

(** {1 Search page} *)

let insert_packages_search : packages_jsoo t -> unit  = 
  fun (packages : packages_jsoo t) -> 
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let pkg = Html.createLi document in
       set_attr pkg "class" (js "package");
       (* Append opam package name *)
       let pkg_name = Html.createA document in
       set_attr pkg_name "class" (js "digodoc-opam");
       set_attr pkg_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.name;
       Dom.appendChild pkg_name name;
       Dom.appendChild pkg pkg_name;
       append_inner pkg @@ concat (js " ") elt##.synopsis;
       (* Insert line in list *)
       let pkg_set = get_element_by_id "results-list" in
       Dom.appendChild pkg_set pkg;
    )
    packages
(** [insert_packages_search pkgs] constructs search page and inserts array of packages [pkgs] that stores 
    converted to js package objects. *)

let insert_libraries_search : libraries_jsoo t -> unit  = 
  fun (libraries : libraries_jsoo t) -> 
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let lib = Html.createLi document in
       set_attr lib "class" (js "package");
       (* Append library name *)
       let lib_name = Html.createA document in
       set_attr lib_name "class" (js "digodoc-lib");
       set_attr lib_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.name;
       Dom.appendChild lib_name name;
       Dom.appendChild lib lib_name;
       (* Append library package *)
       append_inner lib (js " in opam ");
       let lib_opam = Html.createA document in
       set_attr lib_opam "class" (js "digodoc-opam");
       set_attr lib_opam "href" elt##.opampath;
       append_inner lib_opam elt##.opam;
       Dom.appendChild lib lib_opam;
       let lib_set = get_element_by_id "results-list" in
       Dom.appendChild lib_set lib;
    )
    libraries
(** Same as [insert_packages_search] but instead deals with libraries. *)

let insert_modules_search : modules_jsoo t -> unit  = 
  fun (modules : modules_jsoo t) -> 
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let mdl = Html.createLi document in
       set_attr mdl "class" (js "package");
       (* Append module name *)
       let mdl_name = Html.createA document in
       set_attr mdl_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.name;
       Dom.appendChild mdl_name name;
       Dom.appendChild mdl mdl_name;
       (* Append module package *)
       append_inner mdl (js " in opam ");
       let mdl_opam = Html.createA document in
       set_attr mdl_opam "class" (js "digodoc-opam");
       set_attr mdl_opam "href" elt##.opampath;
       append_inner mdl_opam elt##.opam;
       Dom.appendChild mdl mdl_opam;
       (* Append module libraries *)
       if elt##.libs##.length > 0 then append_inner mdl (js " in libs ");
       foreach
         (fun i lib -> 
            let mdl_libs = Html.createA document in
            set_attr mdl_libs "class" (js "digodoc-lib");
            set_attr mdl_libs "href" lib##._1;
            append_inner mdl_libs lib##._0;
            Dom.appendChild mdl mdl_libs;
            if i+1 < elt##.libs##.length then append_inner mdl (js ", ")
         )
         elt##.libs;
       let mdl_set = get_element_by_id "results-list" in
       Dom.appendChild mdl_set mdl
    )
    modules
(** Same as [insert_packages_search] but instead deals with modules. *)

let insert_metas_search : metas_jsoo t -> unit  = 
  fun (metas : metas_jsoo t) -> 
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let meta = Html.createLi document in
       set_attr meta "class" (js "package");
       (* Append meta name *)
       let meta_name = Html.createA document in
       set_attr meta_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.namemeta;
       Dom.appendChild meta_name name;
       Dom.appendChild meta meta_name;
       (* Append meta package *)
       append_inner meta (js " in opam ");
       let meta_opam = Html.createA document in
       set_attr meta_opam "class" (js "digodoc-opam");
       set_attr meta_opam "href" elt##.opampath;
       append_inner meta_opam elt##.opam;
       Dom.appendChild meta meta_opam;
       let meta_set = get_element_by_id "results-list" in
       Dom.appendChild meta_set meta;
    )
    metas
(** Same as [insert_packages_search] but instead deals with metas. *)

let insert_sources_search : sources_jsoo t -> unit  = 
  fun (sources : sources_jsoo t) -> 
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let src = Html.createLi document in
       set_attr src "class" (js "package");
       (* Append source name *)
       let src_name = Html.createA document in
       set_attr src_name "href" elt##.path;
       let name = Html.createCode document in
       append_inner name elt##.namesrc;
       Dom.appendChild src_name name;
       Dom.appendChild src src_name;
       (* Append source package *)
       append_inner src (js " in opam ");
       let src_opam = Html.createA document in
       set_attr src_opam "class" (js "digodoc-opam");
       set_attr src_opam "href" elt##.opampath;
       append_inner src_opam elt##.opam;
       Dom.appendChild src src_opam;
       let src_set = get_element_by_id "results-list" in
       Dom.appendChild src_set src;
    )
    sources
(** Same as [insert_packages_search] but instead deals with sources. *)

let insert_vals_search : vals_jsoo t -> unit  = 
  fun (vals : vals_jsoo t) ->
  foreach 
    (fun _ elt -> 
       (* Create a line *)
       let vall = Html.createLi document in
       set_attr vall "class" (js "package");
       (* Append keyword 'val' *)
       let vall_word = Html.createSpan document in
       set_attr vall_word "class" (js "keyword");
       append_inner vall_word (js "val ");
       Dom.appendChild vall vall_word;
       (* Append val name *)
       let vall_ident = Html.createA document in
       let vall_href = concat (js "#val-") elt##.ident in 
       set_attr vall_ident "href" @@ concat elt##.mdlpath vall_href;
       set_attr vall_ident "class" (js "val");
       append_inner vall_ident elt##.ident;
       Dom.appendChild vall vall_ident;
       append_inner vall (js " : ");
       (* Append val type *)
       let vall_val = Html.createSpan document in
       set_attr vall_val "class" (js "type-annot");
       append_inner vall_val elt##.value;
       Dom.appendChild vall vall_val;
       append_inner vall (js " in opam ");
       (* Append val package *)
       let vall_opam = Html.createA document in
       set_attr vall_opam "class" (js "digodoc-opam");
       set_attr vall_opam "href" elt##.opampath;
       append_inner vall_opam elt##.opam;
       Dom.appendChild vall vall_opam;
       (* Append val module *)
       append_inner vall (js " in ");
       let vall_mdl = Html.createA document in
       set_attr vall_mdl "href" elt##.mdlpath;
       append_inner vall_mdl elt##.mdl;
       Dom.appendChild vall vall_mdl;
       let vall_set = get_element_by_id "results-list" in
       Dom.appendChild vall_set vall;
    )
    vals
(** Same as [insert_packages_search] but instead deals with OCaml values. *)

let insert_types_search : types_jsoo t -> unit =
  fun (types : types_jsoo t) ->
  foreach
    (fun _ elt ->
       (* Create a line *)
       let typel = Html.createLi document in
       set_attr typel "class" (js "package");
       (* Append keyword 'type' *)
       let typel_word = Html.createSpan document in
       set_attr typel_word "class" (js "keyword");
       append_inner typel_word (js "type ");
       Dom.appendChild typel typel_word;
       (* Append type name *)
       let typel_ident = Html.createA document in
       let typel_href = concat (js "#type-") elt##.ident in
       set_attr typel_ident "href" @@ concat elt##.mdlpath typel_href;
       set_attr typel_ident "class" (js "val");
       append_inner typel_ident elt##.ident;
       Dom.appendChild typel typel_ident;
       (* Append type package *)
       append_inner typel (js " in opam ");
       let typel_opam = Html.createA document in
       set_attr typel_opam "class" (js "digodoc-opam");
       set_attr typel_opam "href" elt##.opampath;
       append_inner typel_opam elt##.opam;
       Dom.appendChild typel typel_opam;
       (* Append type module *)
       append_inner typel (js " in ");
       let typel_mdl = Html.createA document in
       set_attr typel_mdl "href" elt##.mdlpath;
       append_inner typel_mdl elt##.mdl;
       Dom.appendChild typel typel_mdl;
       let result_ul = get_element_by_id "results-list" in
       Dom.appendChild result_ul typel;
    )
    types
(** Same as [insert_vals_search] but deals with Ocaml types. *)

let insert_classes_search : classes_jsoo t -> unit =
  fun (classes : classes_jsoo t) ->
  foreach
    (fun _ elt ->
       (* Create a line *)
       let classl = Html.createLi document in
       set_attr classl "class" (js "package");
       (* Append keyword 'class' *)
       let classl_word = Html.createSpan document in
       set_attr classl_word "class" (js "keyword");

       if elt##.isclasstype > 0
       then append_inner classl_word (js "class type ")
       else append_inner classl_word (js "class ");

       Dom.appendChild classl classl_word;
       (* Append class name *)
       let classl_ident = Html.createA document in
       (* let classl_href = concat (js "#class-") elt##.ident in *)

       let classl_href = ref (js "") in

       if elt##.isclasstype > 0
       then classl_href := concat (js "#class-type-") elt##.ident
       else classl_href := concat (js "#class-") elt##.ident;

       set_attr classl_ident "href" @@ concat elt##.mdlpath !classl_href;
       set_attr classl_ident "class" (js "val");
       append_inner classl_ident elt##.ident;
       Dom.appendChild classl classl_ident;
       (* Append class package *)
       append_inner classl (js " in opam ");
       let classl_opam = Html.createA document in
       set_attr classl_opam "class" (js "digodoc-opam");
       set_attr classl_opam "href" elt##.opampath;
       append_inner classl_opam elt##.opam;
       Dom.appendChild classl classl_opam;
       (* Append class module *)
       append_inner classl (js " in ");
       let classl_mdl = Html.createA document in
       set_attr classl_mdl "href" elt##.mdlpath;
       append_inner classl_mdl elt##.mdl;
       Dom.appendChild classl classl_mdl;
       let result_ul = get_element_by_id "results-list" in
       Dom.appendChild result_ul classl;
    )
    classes
(** Same as [insert_types_search] but deals with Ocaml classes. *)

let display_page_info {active_ind; pages; total_number} =
  let s i = string_of_int i  in
  (* Get interval of entries presented on an active result page *)
  let { interval=(f,l); _ } = List.nth pages active_ind in
  (* Create a "Displaying" line *)
  let interval = Html.createB document in
  (* Append entries interval *)
  append_inner interval @@ js @@ s f ^ "-" ^ s l;
  (* Append total entries number *)
  let total = Html.createB document in
  append_inner total @@ js @@ s total_number;
  let page_info_div = get_element_by_id "page-info"in
  append_inner page_info_div (js "Displaying ");
  Dom.appendChild page_info_div interval;
  append_inner page_info_div (js " of ");
  Dom.appendChild page_info_div total;
  append_inner page_info_div (js " total results")
(** [display_page_info pi] extracts inforamation from [pi] of type [Globals.pagination_info]
    and displays information about interval and total number of entries on the page *)

let insert_pagination ({active_ind; pages; _} as pagination) =
  (* Display information about active page *)
  display_page_info pagination;
  let has_precedent = active_ind > 0
  and has_next = active_ind < (List.length pages) - 1 in
  (* If it isn't the first page *)
  if has_precedent
  then begin
    (* Display arrow left *)
    let arrowL = get_element_by_id "previous-page" in
    arrowL##.style##.display := js "";
    let {href; _} = List.nth pages (active_ind - 1) in
    set_attr arrowL "href" (js href)
  end;
  (* If it isn't the last page *)
  if has_next
  then begin
    (* Display arrow right *)
    let arrowR = get_element_by_id "next-page" in
    arrowR##.style##.display := js "";
    let {href; _} = List.nth pages (active_ind + 1) in
    set_attr arrowR "href" (js href)
  end;
  let pages_ol = get_element_by_id "pages" in
  (* Insert computed previously range of pages *)
  List.iteri (fun i { num; href; _} ->
      let page_li = Html.createLi document in
      let page_a = Html.createA document in
      set_attr page_a "href" (js href);
      if i = active_ind 
      then set_attr page_a "class" (js "active");
      append_inner page_a @@ js @@ string_of_int num;
      Dom.appendChild page_li page_a;
      Dom.appendChild pages_ol page_li
    )
    pages
(* [insert_pagination pi] uses calculated [pi] pagination info about current page to display :
   - page information (range of entries id, total number of entries)
   - navigation bar to jump between pages *)

let insert_entries_search entries =
  match entries with
  | Opam packages -> insert_packages_search (Objects.packages_to_jsoo packages)
  | Lib libraries -> insert_libraries_search (Objects.libraries_to_jsoo libraries)
  | Mdl modules -> insert_modules_search (Objects.modules_to_jsoo modules)
  | Meta metas -> insert_metas_search (Objects.metas_to_jsoo metas)
  | Src sources -> insert_sources_search (Objects.sources_to_jsoo sources)
(** Calls specific to [entries] insertion function for search page *)

let insert_elements_search elements =
  match elements with
  | Val vals -> insert_vals_search (Objects.vals_to_jsoo vals)
  | Type types -> insert_types_search (Objects.types_to_jsoo types)
  | Class classes -> insert_classes_search (Objects.classes_to_jsoo classes)
(** Calls specific to [elements] insertion function for search page *)

let insert_modsUl_li : modules_jsoo t -> unit  = 
  fun (modules : modules_jsoo t) ->
  let modsUl = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "modsUl" in
  let input = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextmodules" in
  let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "mod_tag_container" in
  (* Start by removing all children from packsUl and replace them with result of new request*) 
  modsUl##.innerHTML := js "";

  let cur_tags = ref StringSet.empty in
  if to_bool tag_container##hasChildNodes
  then
    begin
      let chosen_tags = tag_container##.childNodes in
      for i = 0 to chosen_tags##.length - 1
      do
        let tag_li = unopt @@ Html.CoerceTo.element @@ unopt @@ (chosen_tags##item i) in
        cur_tags := StringSet.add (to_string (tag_li##.innerText)) !cur_tags;
      done
    end;
    
  foreach
    (fun i elt ->
       if i < 10
       then begin
         let pack_li = Html.createLi document in
         let pack_name = match String.index_opt (to_string elt##.opam) '.' with
           | Some i -> String.sub (to_string elt##.opam) 0 i
           | None -> to_string elt##.opam in
         let name = to_string (concat elt##.name (js @@ ":" ^ pack_name)) in
         pack_li##.onclick := Html.handler (fun _ ->
             if (StringSet.mem name !cur_tags)
             then Html.window##alert (js ("Error : package " ^ name ^ " already chosen,\nCheck for a different version"))
             else 
               begin
                 cur_tags := StringSet.add name !cur_tags;
                 let sp1 = Html.createSpan document in
                 let sp2 = Html.createSpan document in
                 sp1##.classList##add (js "tag"); 
                 sp1##.innerText := js name;
                 sp2##.classList##add (js "remove");
                 sp2##.onclick := Html.handler (fun _ ->
                     cur_tags := StringSet.remove name !cur_tags;
                     Dom.removeChild (unopt @@ sp1##.parentNode) sp1;
                     _false
                   );
                 let tag_container_li = Html.createLi document in
                 Dom.appendChild sp1 sp2;
                 Dom.appendChild tag_container_li sp1;
                 Dom.appendChild tag_container tag_container_li;
               end;
             input##.value := js "";
             modsUl##.style##.display := js "none";
             Headfoot.footerHandler();
             _false
           );
         let a_li = Html.createA document in
         set_attr a_li "href" (js ("#"));
         let in_w = Html.createSpan document in
         set_attr in_w "style" (js "color:black");
         in_w##.innerHTML := js " in ";
         let pkg = Html.createSpan document in 
         set_attr pkg "style" (js "color:green");
         pkg##.innerHTML := js pack_name;
         a_li##.innerHTML := elt##.name;
         Dom.appendChild a_li in_w;
         Dom.appendChild a_li pkg;
         pack_li##.style##.display := js "block";
         Dom.appendChild pack_li a_li;
         Dom.appendChild modsUl pack_li;
         Headfoot.footerHandler();
       end;
    )
    modules
(** preview modules propositions from which to choose *)

let insert_packsUl_li : packages_jsoo t -> unit  = 
  fun (packages : packages_jsoo t) ->
  let packsUl = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "packsUl" in
  let input = unopt @@ Html.CoerceTo.input @@ get_element_by_id "ftextpackages" in
  let tag_container = unopt @@ Html.CoerceTo.ul @@ get_element_by_id "pack_tag_container" in
  (* Start by removing all children from packsUl and replace them with result of new request 
     packsUl##.innerHTML = "";*)
  packsUl##.innerHTML := js "";

  let cur_tags = ref StringSet.empty in
  if to_bool tag_container##hasChildNodes
  then
    begin
      let chosen_tags = tag_container##.childNodes in
      for i = 0 to chosen_tags##.length - 1
      do
        let tag_li = unopt @@ Html.CoerceTo.element @@ unopt @@ (chosen_tags##item i) in
        cur_tags := StringSet.add (to_string (tag_li##.innerText)) !cur_tags;
      done
    end;
  foreach
    (fun i elt ->
       if i < 10
       then begin
         let pack_li = Html.createLi document in
         let name = to_string elt##.name in 
         pack_li##.onclick := Html.handler (fun _ ->
             if (StringSet.mem name!cur_tags)
             then Html.window##alert (js ("Error : package " ^ name ^ " already chosen,\nCheck for a different version"))
             else 
               begin
                 cur_tags := StringSet.add name !cur_tags;
                 let sp1 = Html.createSpan document in
                 let sp2 = Html.createSpan document in
                 sp1##.classList##add (js "tag"); 
                 sp1##.innerText := js name;
                 sp2##.classList##add (js "remove");
                 sp2##.onclick := Html.handler (fun _ ->
                     cur_tags := StringSet.remove name !cur_tags;
                     Dom.removeChild (unopt @@ sp1##.parentNode) sp1;
                     _false
                   );
                 let tag_container_li = Html.createLi document in
                 Dom.appendChild sp1 sp2;
                 Dom.appendChild tag_container_li sp1;
                 Dom.appendChild tag_container tag_container_li;
               end;
             input##.value := js "";
             packsUl##.style##.display := js "none";
             Headfoot.footerHandler();
             _false
           );
         let a_li = Html.createA document in
         set_attr a_li "href" (js ("#"));
         set_attr a_li "style" (js "color:green");
         a_li##.innerText := js name;
         pack_li##.style##.display := js "block";
         Dom.appendChild pack_li a_li;
         Dom.appendChild packsUl pack_li;
         Headfoot.footerHandler();
       end;
    )
    packages
(** preview packages propositions from which to choose *)

let insert_Fulltext_Sources : sources_search_result_jsoo t -> bool -> unit =
  fun (result : sources_search_result_jsoo t) load_more ->
  let load_more_btn = unopt @@ Html.CoerceTo.button @@ get_element_by_id "load_more" in
  let current_pattern = unopt @@ Html.CoerceTo.input @@ get_element_by_id "fpattern_fulltext" in
  let result_div = unopt @@ Html.CoerceTo.div @@ get_element_by_id "result-div" in
  let res_ol = unopt @@ Html.CoerceTo.ol @@ get_element_by_id "results-list" in
  let page_info = unopt @@ Html.CoerceTo.div @@ get_element_by_id "page-info" in
  let occurences_text = Html.createP document in
  let msg_div = unopt @@ Html.CoerceTo.div @@ get_element_by_id "noresult" in

  if not load_more
  then
    begin
      page_info##.innerHTML := js "";
      res_ol##.innerHTML := js "";
      result_div##.style##.display := js "block";
      occurences_text##.innerHTML := js (Printf.sprintf "<b>%d</b> results found for <b>%s<b>" result##.totaloccs (to_string current_pattern##.value##trim));
      occurences_text##.style##.textAlign := js "center";
      Dom.appendChild page_info occurences_text;
    end;

  begin
    foreach
      (fun _ elt ->
         let source_occ_ul = Html.createUl document in
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

         set_attr line2_a_div_tab_tr_td1 "class" (js "occ-position");
         append_inner line2_a_div_tab_tr_td1 (js (string_of_int elt##.occpos));

         set_attr line2_a_div_tab_tr_td2 "class" (js "no_underline");
         append_inner occ_line elt##.occline;
         occ_line##.style##.color := js "black";
         Dom.appendChild line2_a_div_tab_tr_td2 occ_line;

         append_inner line2_a_div_tab_tr (js "&nbsp At line &nbsp");
         line2_a_div_tab_tr##.style##.marginLeft := js "2%";

         Dom.appendChild line2_a_div_tab_tr line2_a_div_tab_tr_td1;
         append_inner line2_a_div_tab_tr (js "&nbsp &nbsp ");
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
  end;
  msg_div##.style##.display := js "none";

  if (to_string current_pattern##.value##trim) = ""
  then
    begin
      msg_div##.style##.display := js "none";
      result_div##.style##.display := js "none";
      load_more_btn##.style##.display := js "none";
    end;

  Headfoot.footerHandler ();
  (** Inserts the result of fulltext search given by ez_search and displays the load more button if [load_more] is
      set to true *)

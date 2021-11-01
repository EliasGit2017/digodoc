(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

open Digodoc_common.Utils

let about_page =
  Printf.sprintf
    {|<!DOCTYPE html>
        <html lang="en">
        <head>
            <title>About</title>
            <link rel="stylesheet" href="static/styles/odoc/odoc.css"/>
            <script type="text/javascript" src="static/scripts/search.js" charset="utf-8"></script>
            <meta charset="utf-8"/>
            <meta name="generator" content="digodoc 0.1"/>
            <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
            <script src="static/scripts/highlight.pack.js"></script>
            <script>hljs.initHighlightingOnLoad();</script>
            <script defer="defer" type="application/javascript" src="static/scripts/headerFooter.js"></script>
        </head>
        <body>
            %s
            <div class="content">
                %s
            </div>
            %s
        </body>
        </html>
        |}
        (file_content "header.html")
        (file_content "about.html")
        (file_content "footer.html")

(* Emulation of doc generation. To use only to check html style / scripts *)
let generate () =
    if EzFile.exists "examples" then 
        EzFile.remove_dir ~all:true "examples";
    EzFile.make_dir ~p:true "examples/sources";

    (* page example for docs: about.html *)
    Process.call [|"rsync"; "-auv"; "html/.";  "examples/." |];
    let brace () var =
        match var with
        | "header_link" -> {| | <a href="#header">To the top</a>|}
        | "search" -> file_content "search_index.html"
        | _ -> ""
    in
    let about_html = Ez_subst.V1.EZ_SUBST.string about_page ~brace ~ctxt:() in
    EzFile.write_file "examples/about.html" about_html;

  (* pages examples for sources: config folder *)
  Htmlize.Main.htmlize "examples/sources/" ["config/"];
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

open Ez_file
open Ez_file.FileString.OP

let file_content filename =
  match Sys.getenv "DIGODOC_CONFIG" with
  | dir when FileString.exists (dir // filename) -> 
    FileString.read_file (dir // filename)
  | exception Not_found | _ ->
    begin   
      match Files.read filename with
      | None -> ""
      | Some file_content -> file_content
    end
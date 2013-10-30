(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Batteries

let fmt = Printf.sprintf

type t =
  { database : string option
  ; host : string option
  ; password : string option
  ; user : string option
  ; port : int option
  ; unix_domain_socket_dir : string option
  }

let fail_attrib ~tag x =
  let msg = fmt "Unexpected attribute '%s' inside '%s'" x tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_content ~tag =
  let msg = fmt "Unexpected content inside '%s'" tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_tag ~tag =
  let msg = fmt "Unexpected tag '%s'" tag in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let fail_pcdata x =
  let msg = fmt "Unexpected pcdata '%s' inside cumulus" x in
  raise (Ocsigen_extensions.Error_in_config_file msg)

let rec init_fun data = function
  | Simplexmlparser.Element ("database" as tag, attribs, content)::l ->
      if content <> [] then
        fail_content ~tag;
      let data =
        List.fold_left
          (fun data -> function
             | "name", x -> {data with database = Some x}
             | "user", x -> {data with user = Some x}
             | "host", x -> {data with host = Some x}
             | "port", x ->
                 begin
                   try {data with port = Some (int_of_string x)} with
                   | Failure _ -> fail_attrib ~tag "port"
                 end
             | "socket-dir", x -> {data with unix_domain_socket_dir = Some x}
             | "password-file", x ->
                 let x =
                   let file = open_in x in
                   finally (fun () -> close_in file) input_line file
                 in
                 {data with password = Some x}
             | x, _ -> fail_attrib ~tag x
          )
          data
          attribs
      in
      init_fun data l
  | Simplexmlparser.Element (tag, _, _)::_ -> fail_tag ~tag
  | Simplexmlparser.PCData pcdata :: _ ->
      fail_pcdata pcdata
  | [] -> data

let {database; host; password; user; port; unix_domain_socket_dir} =
  let data =
    { database = None
    ; host = None
    ; password = None
    ; user = None
    ; port = None
    ; unix_domain_socket_dir = None
    }
  in
  let c = Eliom_config.get_config () in
  init_fun data c

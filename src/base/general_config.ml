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
open Eliom_lib.Lwt_ops

let rec init_fun data = function
  | Simplexmlparser.Element ("general" as tag, attribs, content)::l ->
      if content <> [] then
        Configfile.fail_content ~tag;
      let data =
        List.fold_left
          (fun _data -> function
             | "email", x -> Some x
             | x, _ -> Configfile.fail_attrib ~tag x
          )
          data
          attribs
      in
      init_fun data l
  | Simplexmlparser.Element (tag, _, _)::_ -> Configfile.fail_tag ~tag
  | Simplexmlparser.PCData pcdata :: _ ->
      Configfile.fail_pcdata pcdata
  | [] -> data

let email =
  let data = None in
  let c = Eliom_config.get_config () in
  Option.default_delayed
    (fun () -> Configfile.fail_missing ~tag:"general" "email")
    (init_fun data c)

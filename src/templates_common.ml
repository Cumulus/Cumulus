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
open Eliom_content.Html5.F

let string_input_box ?(a=[]) =
  string_input ~a:(a_class ["input-box"] :: a)

let submit_input ?(a=[]) =
  string_input
    ~a:(a_class ["btn-box"] :: a)
    ~input_type:`Submit

let links_of_tags tags =
  List.fold_left (fun acc tag ->
    let link =
      a
        ~a:[a_class ["tags"]]
        ~service:Services.tag_feed
        [pcdata tag]
        tag
    in
    acc @ [pcdata " "; link]
  ) [] tags

module Markdown = MarkdownHTML.Make_html5(struct
  include Raw
  module Svg = Eliom_content.Svg.F.Raw
end)

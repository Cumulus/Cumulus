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

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

val event : unit Eliom_react.Down.t

val to_html :
  Db_feed.feeds_and_tags -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val comments_to_html :
  int32 -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t
val branch_to_html :
  int32 -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t
val to_atom : unit -> Atom_feed.feed Lwt.t
val append_feed : (string * (string * string)) -> append_state Lwt.t
val append_link_comment : (int * (string * (string * string))) -> append_state Lwt.t
val append_desc_comment : (int * string) -> append_state Lwt.t
val feed_id_to_html : int32 -> (([> Html5_types.div ] Html.elt) list) Lwt.t

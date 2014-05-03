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

val main :
  unit ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val user :
  string ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val fav_feed :
  string ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val tag :
  string ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val register : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val view_feed : int32 -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val preferences : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val comment : int32 -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val edit_feed : int32 -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val reset_password : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val to_atom : unit -> Atom_feed.feed Lwt.t

val comments_to_atom : unit -> Atom_feed.feed Lwt.t

val tree_to_atom : int32 -> Atom_feed.feed Lwt.t

val tag_to_atom : string -> Atom_feed.feed Lwt.t

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

open CalendarLib

type feed = Db_feed_ng.feed =
  { author : int32
  ; id : int32
  ; date : CalendarLib.Calendar.t
  ; description : string
  ; url : string option
  ; parent: int32 option
  ; root : int32 option
  ; tags : string list
  ; score : int
  }

val to_html : Db_feed_ng.feed ->
  (([> `A of [> `PCDATA ] | `Br | `Div | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : Db_feed_ng.feed -> Atom_feed.entry Lwt.t

val get_edit_infos : int32 ->
  (bool * string * string * string) Lwt.t

val delete_feed_check :
  feed:int32 ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val add_fav : int32 -> unit Lwt.t
val del_fav : int32 -> unit Lwt.t
val upvote : int32 -> unit Lwt.t
val downvote : int32 -> unit Lwt.t
val cancel_vote : int32 -> unit Lwt.t

(* TODO: Remove the following functions *)
val get_root_feeds : Db_feed_ng.feed_generator
val count_root_feeds : unit -> int64 Lwt.t
val get_feeds_with_author : string -> Db_feed_ng.feed_generator
val count_feeds_with_author : string -> int64 Lwt.t
val get_feeds_with_tag : string -> Db_feed_ng.feed_generator
val count_feeds_with_tag : string -> int64 Lwt.t
val get_fav_with_username : string -> Db_feed_ng.feed_generator
val count_fav_with_username : string -> int64 Lwt.t
val exist : feedid:int32 -> unit -> bool Lwt.t
val is_feed_author : feed:int32 -> userid:int32 -> unit -> bool Lwt.t

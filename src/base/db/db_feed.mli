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

type feed =
  { author : int32
  ; id : int32
  ; date : CalendarLib.Calendar.t
  ; description : string
  ; url : string option
  ; parent: int32 option
  ; root : int32 option
  ; tags : string list
  ; score : int
  ; user : < email_digest : string; name : string >
  ; fav : bool
  ; vote : int
  ; count : int
  ; leftBound : int32
  ; rightBound : int32
  }

type feeds = feed list

type feed_generator =
  starting:int32 ->
  number:int32 ->
  user:int32 option ->
  unit ->
  feeds Lwt.t

val get_tree_feeds : int32 -> feed_generator
val get_links_feeds : feed_generator
val get_comments_feeds : feed_generator
val get_root_feeds : feed_generator
val get_feeds : feed_generator
val get_feeds_with_author : string -> feed_generator
val get_feeds_with_tag : string -> feed_generator
val get_feed_with_id : user:int32 option -> int32 -> feed Lwt.t
val get_feed_with_url : user:int32 option -> string -> feed option Lwt.t
val get_fav_with_username : string -> feed_generator

val get_comments :
  user:int32 option ->
  int32 ->
  feeds Lwt.t

val get_feeds_of_interval :
  user:int32 option ->
  int32 -> int32 ->
  feeds Lwt.t

val is_feed_author :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  bool Lwt.t

val add_feed :
  ?root:int32 ->
  ?parent:int32 ->
  ?url:string ->
  description:string ->
  tags:string list ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val delete_feed :
  feedid:int32 ->
  unit ->
  unit Lwt.t

val add_fav :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val del_fav :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val upvote :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  [> `Ok of (int * int)] Lwt.t

val downvote :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  [> `Ok of (int * int)] Lwt.t

val cancelvote :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  [> `Ok of (int * int)] Lwt.t

val update :
  feedid:int32 ->
  url:string option ->
  description:string ->
  tags:string list ->
  unit ->
  unit Lwt.t

val exists : feedid:int32 -> unit -> bool Lwt.t
val exists_with_url : url:string -> bool Lwt.t

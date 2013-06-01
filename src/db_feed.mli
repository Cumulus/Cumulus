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

class type feed = object
  method author : (Sql.int32_t, Sql.non_nullable) Db.t
  method id : (Sql.int32_t, Sql.non_nullable) Db.t
  method timedate : (Sql.timestamp_t, Sql.non_nullable) Db.t
  method description : (Sql.string_t, Sql.non_nullable) Db.t
  method url : (Sql.string_t, Sql.nullable) Db.t
  method parent : (Sql.int32_t, Sql.nullable) Db.t
  method root : (Sql.int32_t, Sql.nullable) Db.t
end

class type tag = object
  method tag : (Sql.string_t, Sql.non_nullable) Db.t
  method id_feed : (Sql.int32_t, Sql.non_nullable) Db.t
end

class type fav = object
  (* method id : (Sql.int32_t, Sql.non_nullable) Db.t *)
  method id_user : (Sql.int32_t, Sql.non_nullable) Db.t
  method id_feed : (Sql.int32_t, Sql.non_nullable) Db.t
end

type feeds_and_tags = feed list * tag list
type feed_generator =
    starting:int32 ->
    number:int32 ->
    unit ->
    feeds_and_tags Lwt.t

val get_tree_feeds : int32 -> feed_generator
val get_links_feeds : feed_generator
val get_root_feeds : feed_generator
val get_feeds : feed_generator
val get_feeds_with_author : string -> feed_generator
val get_feeds_with_tag : string -> feed_generator
val get_fav_with_username : string -> feed_generator
val get_feed_url_with_url :
  string ->
  < url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t >
    option Lwt.t
val get_feed_with_url :
  string -> feed option Lwt.t
val get_feed_with_id :
  int32 ->
  (feed * tag list) Lwt.t
val count_feeds :
  unit ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val count_root_feeds :
  unit ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val count_feeds_with_author :
  string ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val count_fav_with_username :
  string ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val count_feeds_with_tag :
  string ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val count_comments :
  int32 ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val get_comments :
  int32 ->
  feeds_and_tags Lwt.t

val add_feed :
  ?root:int32 ->
  ?parent:int32 ->
  ?url:string ->
  description:string ->
  tags:string list ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val is_feed_author :
  feed:int32 ->
  userid:int32 ->
  unit ->
  bool Lwt.t

val delete_feed :
  feed:int32 ->
  userid:int32 ->
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
  unit Lwt.t

val downvote :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val cancelvote :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  unit Lwt.t

val user_vote :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  int32 Lwt.t

val score :
  feedid:int32 ->
  unit ->
  int32 Lwt.t

val is_fav :
  feedid:int32 ->
  userid:int32 ->
  unit ->
  bool Lwt.t

val is_url :
  feedid:int32 ->
  unit ->
  bool Lwt.t

val is_root :
  feedid:int32 ->
  unit ->
  bool Lwt.t

val get_root :
  feedid:int32 ->
  unit->
  feed option Lwt.t

val update :
  feedid:int32 ->
  url:string option ->
  description:string ->
  tags:string list ->
  unit ->
  unit Lwt.t

val exist :
  feedid:int32 ->
  unit ->
  bool Lwt.t

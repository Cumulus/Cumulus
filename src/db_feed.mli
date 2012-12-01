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

type feeds_and_tags = feed list * tag list
type feed_generator =
    starting:int32 ->
    number:int32 ->
    unit ->
    feeds_and_tags Lwt.t

val get_root_feeds : feed_generator
val get_feeds : feed_generator
val get_feeds_with_author : string -> feed_generator
val get_feeds_with_tag : string -> feed_generator
val get_feed_url_with_url :
  string ->
  < url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t >
    option Lwt.t
val get_feed_with_id :
  int32 ->
  (feed * tag list) Lwt.t
val count_feeds :
  unit ->
  < n : (Sql.int64_t, Sql.non_nullable) Db.t > Lwt.t
val count_feeds_with_author :
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
  string ->
  string ->
  string list ->
  int32 ->
  unit Lwt.t
val add_desc_comment :
  string -> int32 -> int32 -> int32 -> unit Lwt.t

val is_feed_author : int32 -> int32 -> bool Lwt.t
val delete_feed : int32 -> int32 -> unit Lwt.t

class type ['a] macaque_type = object
  method get : unit
  method nul : Sql.non_nullable
  method t : 'a
end

class type feed = object
  method author : Sql.int32_t macaque_type Sql.t
  method id : Sql.int32_t macaque_type Sql.t
  method timedate : Sql.timestamp_t macaque_type Sql.t
  method description : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t
  method parent : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t
  method root : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t
end

class type tag = object
  method tag : Sql.string_t macaque_type Sql.t
  method id_feed : Sql.int32_t macaque_type Sql.t
end

type feeds_and_tags = feed list * tag list

val get_user_name_and_email_with_id :
  int32 ->
  < email : Sql.string_t macaque_type Sql.t;
  name : Sql.string_t macaque_type Sql.t >
    Lwt.t
val get_user_with_name :
  string ->
  < email : Sql.string_t macaque_type Sql.t;
  id : Sql.int32_t macaque_type Sql.t;
  name : Sql.string_t macaque_type Sql.t;
  password : Sql.string_t macaque_type Sql.t;
  is_admin : Sql.bool_t macaque_type Sql.t >
    option Lwt.t
val get_feeds :
  ?starting:int32 ->
  ?number:int32 ->
  unit ->
  feeds_and_tags Lwt.t
val get_feeds_with_author :
  ?starting:int32 ->
  ?number:int32 ->
  string ->
  feeds_and_tags Lwt.t
val get_feeds_with_tag :
  ?starting:int32 ->
  ?number:int32 ->
  string ->
  feeds_and_tags Lwt.t
val get_feed_url_with_url :
  string ->
  < url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t >
    option Lwt.t
val get_feed_with_id :
  int32 ->
  feeds_and_tags Lwt.t
val count_feeds :
  unit ->
  < n : Sql.int64_t macaque_type Sql.t > Lwt.t
val count_feeds_with_author :
  string ->
  < n : Sql.int64_t macaque_type Sql.t > Lwt.t
val count_feeds_with_tag :
  string ->
  < n : Sql.int64_t macaque_type Sql.t > Lwt.t
val count_comments :
  int32 ->
  < n : Sql.int64_t macaque_type Sql.t > Lwt.t
val get_comments :
  int32 ->
  feeds_and_tags Lwt.t 

val add_feed : string -> string -> string list -> int32 -> unit Lwt.t
val add_user : string -> string -> string -> unit Lwt.t

val is_feed_author : int32 -> int32 -> bool Lwt.t
val delete_feed : int32 -> int32 -> unit Lwt.t

val update_user_password : int32 -> string -> unit Lwt.t
val update_user_email : int32 -> string -> unit Lwt.t

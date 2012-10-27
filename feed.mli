type feed

class type feed_db = object
  method author : Sql.int32_t macaque_type Sql.t
  method id : Sql.int32_t macaque_type Sql.t
  method timedate : Sql.timestamp_t macaque_type Sql.t
  method title : Sql.string_t macaque_type Sql.t
  method url : Sql.string_t macaque_type Sql.t
end

val feed_new : feed_db -> string list -> feed
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

type feed

class type feed_db = object
  method author : Sql.int32_t Db.macaque_type Sql.t
  method id : Sql.int32_t Db.macaque_type Sql.t
  method timedate : Sql.timestamp_t Db.macaque_type Sql.t
  method description : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t
  method parent : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t
end

val feed_new : feed_db -> string list -> feed
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

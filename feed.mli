type feed

class type feed_db = object
  method author : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
  method id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
  method timedate : < get : unit; nul : Sql.non_nullable; t : Sql.timestamp_t > Sql.t
  method title : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method url : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
end

val feed_new : feed_db -> string list -> feed
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

type feed

val feed_new :
  < author : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
  id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t;
  timedate : < get : unit; nul : Sql.non_nullable; t : Sql.timestamp_t > Sql.t;
  title : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t;
  url : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t >
    -> string list -> feed
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

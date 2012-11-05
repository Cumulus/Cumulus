type feed

val feed_new : Db.feed -> string list -> feed
val get_comments_of_feed : feed -> feed list -> feed list
val get_others_of_feed : feed -> feed list -> feed list
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

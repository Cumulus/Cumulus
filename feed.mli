type feed_content
type feed

val feed_new : string -> feed_content -> feed
val feed_new_from_new : string -> string -> feed
val to_html : feed -> ([> `Br | `PCDATA ] Html.elt) list
val to_atom : feed -> Atom_feed.entry
val write : feed -> (string -> feed_content -> unit Lwt.t) -> unit Lwt.t

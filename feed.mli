type feed_content
type feed

val feed_new : string -> feed_content -> feed
val to_html : feed -> ([> `Br | `PCDATA ] Html.elt) list
val to_atom : feed -> Atom_feed.entry

type feeds

val feeds_new : unit -> feeds
val to_html : feeds -> ([> `Html ] Html.elt) Lwt.t
val to_atom : feeds -> Atom_feed.feed Lwt.t
val append_feed : feeds -> Feed.feed -> unit Lwt.t

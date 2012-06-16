type feeds

val feeds_new : unit -> feeds
val to_html : feeds -> (([> Html5_types.p ] Html.elt) list) Lwt.t
val to_atom : feeds -> Atom_feed.feed Lwt.t
val append_feed : feeds -> (string * string * string) -> bool Lwt.t

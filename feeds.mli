type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

val event : unit Eliom_react.Down.t

val to_html : Db.feeds_and_tags -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val to_atom : unit -> Atom_feed.feed Lwt.t
val append_feed : (string * (string * string)) -> append_state Lwt.t
val feed_id_to_html : int32 -> (([> Html5_types.div ] Html.elt) list) Lwt.t

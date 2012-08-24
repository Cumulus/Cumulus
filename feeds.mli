type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

val to_html :
  starting:int32 -> unit -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val author_to_html :
  starting:int32 -> string -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val tag_to_html :
  starting:int32 -> string -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val to_atom : unit -> Atom_feed.feed Lwt.t
val append_feed : (string * (string * string)) -> append_state Lwt.t
val feed_id_to_html : int32 -> (([> Html5_types.div ] Html.elt) list) Lwt.t

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

val event : unit Eliom_react.Down.t

val to_html :
  starting:int32 -> unit -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val author_to_html :
  starting:int32 -> string -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val tag_to_html :
  starting:int32 -> string -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val root_to_html :
  starting:int32 -> unit -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val comments_to_html :
  int32 -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t
val branch_to_html :
  int32 -> int32 -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t
val to_atom : unit -> Atom_feed.feed Lwt.t
val append_feed : (string * (string * string)) -> append_state Lwt.t
val feed_id_to_html : int32 -> (([> Html5_types.div ] Html.elt) list) Lwt.t

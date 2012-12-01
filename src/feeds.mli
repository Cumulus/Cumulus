type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

val event : unit Eliom_react.Down.t

val to_html :
  Db_feed.feeds_and_tags -> (([> Html5_types.div ] Html.elt) list) Lwt.t
val comments_to_html :
  int32 -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t
val branch_to_html :
  int32 -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t
val to_atom : unit -> Atom_feed.feed Lwt.t
val append_feed : (string * (string * string)) -> append_state Lwt.t
val append_link_comment : (int * (string * (string * string))) -> append_state Lwt.t
val append_desc_comment : (int * string) -> append_state Lwt.t
val feed_id_to_html : int32 -> (([> Html5_types.div ] Html.elt) list) Lwt.t
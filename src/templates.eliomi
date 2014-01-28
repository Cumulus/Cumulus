val main :
  ?page:int ->
  service:'a ->
  unit ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val user :
  ?page:int ->
  service:'a ->
  string ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val fav_feed :
  ?page:int->
  service:'a ->
  string ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val tag :
  ?page:int ->
  service:'a ->
  string ->
  [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val register : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val view_feed : int -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val preferences : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val comment : int -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val edit_feed : int -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t
val reset_password : unit -> [> `Html ] Eliom_content.Html5.F.elt Lwt.t

val to_atom : unit -> Atom_feed.feed Lwt.t
val comments_to_atom : unit -> Atom_feed.feed Lwt.t
val tree_to_atom : int32 -> unit -> Atom_feed.feed Lwt.t
val tag_to_atom: string -> unit -> Atom_feed.feed Lwt.t

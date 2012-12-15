val main :
  ?page:int ->
  service:'a ->
  unit ->
  [> `Html ] Html.elt Lwt.t
val user :
  ?page:int ->
  service:'a ->
  string ->
  [> `Html ] Html.elt Lwt.t
val tag :
  ?page:int ->
  service:'a ->
  string ->
  [> `Html ] Html.elt Lwt.t
val register : unit -> [> `Html ] Html.elt Lwt.t
val view_feed : int -> [> `Html ] Html.elt Lwt.t
val preferences : unit -> [> `Html ] Html.elt Lwt.t
val comment : int -> [> `Html ] Html.elt Lwt.t

val main : ?page:int -> [< Html5_types.body_content_fun > `Br `Footer `Form `P ]
  Html.elt list -> [> `Html ] Html.elt Lwt.t
val user : ?page:int -> [< Html5_types.body_content_fun > `Br `Footer `Form `P ]
  Html.elt list -> string -> [> `Html ] Html.elt Lwt.t
val tag : ?page:int -> [< Html5_types.body_content_fun > `Br `Footer `Form `P ]
  Html.elt list -> string -> [> `Html ] Html.elt Lwt.t
val register : [> `Html ] Html.elt Lwt.t
val view_feed : int -> [> `Html ] Html.elt Lwt.t

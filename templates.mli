val main : [< Html5_types.body_content_fun > `Br `Footer `Form `P ] Html.elt list ->
  [> `Html ] Html.elt Lwt.t
val user : [< Html5_types.body_content_fun > `Br `Footer `Form `P ] Html.elt list ->
  string -> [> `Html ] Html.elt Lwt.t

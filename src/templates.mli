val main : [< Html5_types.body_content_fun > `A `Br `Form `P ] Html.elt list ->
  [> `Html ] Html.elt Lwt.t
val user : [< Html5_types.body_content_fun > `A `Br `Form `P ] Html.elt list ->
  string -> [> `Html ] Html.elt Lwt.t

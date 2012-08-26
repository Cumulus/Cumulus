val main : ?page:int ->
  [< Html5_types.div_content_fun > `Br `Div `Footer `Header `P ]
  Html.elt list -> [> `Html ] Html.elt Lwt.t
val user : ?page:int ->
  [< Html5_types.div_content_fun > `Br `Div `Footer `Header `P ]
  Html.elt list -> string -> [> `Html ] Html.elt Lwt.t
val tag : ?page:int ->
  [< Html5_types.div_content_fun > `Br `Div `Footer `Header `P ]
  Html.elt list -> string -> [> `Html ] Html.elt Lwt.t
val register : unit -> [> `Html ] Html.elt Lwt.t
val view_feed : int -> [> `Html ] Html.elt Lwt.t
val preferences :
  [< Html5_types.div_content_fun > `Form `Header `PCDATA ] Html.elt list ->
  [> `Html ] Html.elt Lwt.t

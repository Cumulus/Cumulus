val main : ?page:int -> [< Html5_types.div_content_fun > `Br `Footer `Form `Header `P ]
  Html.elt list -> [> `Html ] Html.elt Lwt.t
val user : ?page:int -> [< Html5_types.div_content_fun > `Br `Footer `Form `Header `P ]
  Html.elt list -> string -> [> `Html ] Html.elt Lwt.t
val tag : ?page:int -> [< Html5_types.div_content_fun > `Br `Footer `Form `Header `P ]
  Html.elt list -> string -> [> `Html ] Html.elt Lwt.t
val register : unit -> [> `Html ] Html.elt Lwt.t
val view_feed : int -> [> `Html ] Html.elt Lwt.t
val preferences : [< Html5_types.div_content_fun > `Div `Form `PCDATA ] Html.elt list  -> [> `Html ] Html.elt Lwt.t
val user_form : unit -> [> Html5_types.form ] Eliom_content_core.Html5.elt list

type feed
type tree = Sheet of feed | Node of feed * tree list                                                                   

val feed_new : Db.feed -> string list -> feed
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

val generate_tree_comments : tree list -> feed list -> tree list
val string_of_tree : tree -> string
val html_from_tree : tree -> 
  [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t

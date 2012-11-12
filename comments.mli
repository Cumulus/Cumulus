open Feed

type tree = Sheet of Feed.feed | Node of Feed.feed * tree list

val tree_comments : tree list -> feed list -> tree list
val string_of_tree : tree -> string 
val to_html : tree -> [< Html5_types.div_content_fun > `A `Br `Div `Img `PCDATA ] Html.elt Lwt.t

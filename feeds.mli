type feeds

val feeds_new : unit -> feeds
val to_html : feeds -> ([> `Html ] Html.elt) Lwt.t

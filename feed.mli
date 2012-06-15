type feed

val feed_new : string -> (string * CalendarLib.Calendar.t * string) -> feed
val to_html : feed -> ([> `Br | `PCDATA ] Html.elt) list

open CalendarLib

type feed = {
  id : int32;
  url : string option;
  description : string;
  date : CalendarLib.Calendar.t;
  author : int32;
  parent : int32 option;
  root : int32 option;
  tags: string list
}

val feed_new : Db.feed -> string list -> feed
val to_html : feed ->
  (([> `A of [> `PCDATA ] | `Br | `Img | `PCDATA ] Html.elt) list) Lwt.t
val to_atom : feed -> Atom_feed.entry Lwt.t

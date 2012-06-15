module Xml = Eliom_content_core.Xml

type feed_content = {
  title : string;
  date : CalendarLib.Calendar.t;
  author : string
}

type feed = {
  url : string;
  content : feed_content
}

let feed_new url content = {
  url = url;
  content = {
    title = content.title;
    date = content.date;
    author = content.author
  }
}

let to_html self = [
  Html.pcdata ("url: " ^ self.url);
  Html.br ();
  Html.pcdata ("title: " ^ self.content.title);
  Html.br ();
  Html.pcdata ("date: " ^ (Utils.string_of_calendar self.content.date));
  Html.br ();
  Html.pcdata ("author: " ^ self.content.author)
]

let to_atom self =
  Atom_feed.entry
    ~updated: self.content.date
    ~id: (Xml.uri_of_string self.url)
    ~title: (Atom_feed.plain self.content.title)
    []

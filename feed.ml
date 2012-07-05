module Calendar = CalendarLib.Calendar

type feed_content = {
  title : string;
  date : CalendarLib.Calendar.t;
  author : string;
  tags: string list
}

type feed = {
  url : string;
  content : feed_content
}

let feed_new url content = {
  url = url;
  content = content
}

let feed_new_from_new url title author tags = {
  url = url;
  content = {
    title = title;
    date = Calendar.now ();
    author = author;
    tags = tags
  }
}

let to_html self =
  let url_service =
    Eliom_service.external_service
      self.url []
      Eliom_parameter.unit () in [
  Html.a url_service [Html.pcdata self.content.title] ();
  Html.br ();
  Html.pcdata ("date: " ^ (Utils.string_of_calendar self.content.date));
  Html.br ();
  Html.pcdata ("author: " ^ self.content.author);
  Html.br ();
  Html.pcdata
    ("tags: " ^ (List.fold_left (fun a b -> a ^ " " ^ b) "" self.content.tags))
]

let to_atom self =
  Atom_feed.entry
    ~updated: self.content.date
    ~id: (Html.Xml.uri_of_string self.url)
    ~title: (Atom_feed.plain self.content.title)
    [Atom_feed.authors [Atom_feed.author self.content.author]]

let write self f =
  f self.url self.content

let filter_author author = function
  | self when self.content.author = author -> true
  | _ -> false

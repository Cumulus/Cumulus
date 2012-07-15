module Calendar = CalendarLib.Calendar

type feed = {
  id : int32;
  url : string;
  title : string;
  date : Calendar.t;
  author : int32;
  tags: string
}

let feed_new data = {
  id = data#!id;
  url = data#!url;
  title = data#!title;
  date = data#!timedate;
  author = data#!author;
  tags = data#!tags
}

let to_html self =
  let url_service =
    Eliom_service.external_service
      self.url []
      Eliom_parameter.unit () in
  Db.get_user_name_with_id self.author >>= (fun author ->
    Lwt.return [
      Html.a url_service [Html.pcdata self.title] ();
      Html.br ();
      Html.pcdata ("date: " ^ (Utils.string_of_calendar self.date));
      Html.br ();
      Html.pcdata ("author: " ^ author#!name);
      Html.br ();
      Html.pcdata ("tags: " ^ self.tags)
    ]
  )

let to_atom self =
  Db.get_user_name_with_id self.author >>= (fun author ->
    Lwt.return (
      Atom_feed.entry
        ~updated: self.date
        ~id: (Html.Xml.uri_of_string self.url)
        ~title: (Atom_feed.plain self.title)
        [Atom_feed.authors [Atom_feed.author author#!name]]
    )
  )

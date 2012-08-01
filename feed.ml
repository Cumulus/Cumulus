module Calendar = CalendarLib.Calendar

type feed = {
  id : int32;
  url : string;
  title : string;
  date : Calendar.t;
  author : int32;
  tags: string list
}

let feed_new data tags = {
  id = data#!id;
  url = data#!url;
  title = data#!title;
  date = data#!timedate;
  author = data#!author;
  tags = tags
}

let links_of_tags tags =
  List.fold_left (fun acc tag ->
    let link = Html.a Services.tag_feed [Html.pcdata (tag)] tag in
    acc @ [Html.pcdata " "; link]
  ) [] tags

let to_html self =
  let url_service =
    Eliom_service.external_service
      self.url []
      Eliom_parameter.unit () in
  Db.get_user_name_and_email_with_id self.author >>= (fun author ->
    Lwt.return ([
      Html.img
        ~alt: (author#!name)
        ~src: (Html.make_uri ~service: (Utils.get_gravatar (author#!name)) (80, "identicon"))
      ();
      Html.a url_service [Html.pcdata self.title] ();
      Html.br ();
      Html.pcdata ("date: " ^ (Utils.string_of_calendar self.date));
      Html.br ();
      Html.pcdata ("author: ");
      Html.a Services.author_feed [Html.pcdata (author#!name)] author#!name;
      Html.br ();
      Html.pcdata "tags:"
    ] @ links_of_tags self.tags)
  )

let to_atom self =
  Db.get_user_name_and_email_with_id self.author >>= (fun author ->
    Lwt.return (
      Atom_feed.entry
        ~updated: self.date
        ~id: (Html.Xml.uri_of_string self.url)
        ~title: (Atom_feed.plain self.title)
        [Atom_feed.authors [Atom_feed.author author#!name]]
    )
  )

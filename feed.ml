module Calendar = CalendarLib.Calendar

class type feed_db = object
  method author : Sql.int32_t macaque_type Sql.t
  method id : Sql.int32_t macaque_type Sql.t
  method timedate : Sql.timestamp_t macaque_type Sql.t
  method title : Sql.string_t macaque_type Sql.t
  method url : Sql.string_t macaque_type Sql.t
end

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
    let link = Html.a ~a: [Html.a_class ["tags"]] ~service: Services.tag_feed [Html.pcdata tag] (None, tag) in
    acc @ [Html.pcdata " "; link]
  ) [] tags

let to_html self =
  let url_service =
    Eliom_service.external_service
      self.url []
      Eliom_parameter.unit () in
  Db.get_user_name_and_email_with_id self.author >>= fun author ->
  User.get_userid () >>= (function
    | None -> Lwt.return false
    | Some userid ->
        Db.is_feed_author self.id userid
        >>= fun x ->
        User.is_admin ()
        >>= fun y ->
        Lwt.return (x || y)
  )
  >>= fun is_author ->
  Lwt.return (
    List.flatten [
      [Html.img
          ~a: [Html.a_class ["left"]]
          ~alt: (author#!name)
          ~src: (
            Html.make_uri
              ~service: (Utils.get_gravatar (author#!email)) (40, "identicon")
          )
          ();
       Html.a ~a: [Html.a_class ["postitle"]] ~service: url_service
         [Html.pcdata self.title] ();
       Html.br ();
       Html.pcdata ("Publié le " ^ (Utils.string_of_calendar self.date) ^ " par ");
       Html.a Services.author_feed [Html.pcdata (author#!name)] (None, author#!name);
      ];
      [Html.br ();
       (* TODO : afficher "n commentaire(s)" *)
       Html.a
         Services.view_feed
         [Html.pcdata "n commentaires "]
         (Int32.to_int self.id, self.title);
       Html.pcdata "Tags: "
      ];
      links_of_tags self.tags;
      (if is_author then
          [Html.a Services.delete_feed [Html.pcdata " (supprimer ?)"] self.id]
       else []
      );
    ]
  )

let to_atom self =
  Db.get_user_name_and_email_with_id self.author >>= fun author ->
  Lwt.return (
    Atom_feed.entry
      ~updated: self.date
      ~id: (Html.Xml.uri_of_string self.url)
      ~title: (Atom_feed.plain self.title)
      [Atom_feed.authors [Atom_feed.author author#!name]]
  )

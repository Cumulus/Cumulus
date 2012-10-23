module Calendar = CalendarLib.Calendar

class type feed_db = object
  method author : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
  method id : < get : unit; nul : Sql.non_nullable; t : Sql.int32_t > Sql.t
  method timedate : < get : unit; nul : Sql.non_nullable; t : Sql.timestamp_t > Sql.t
  method description : < get : unit; nul : Sql.non_nullable; t : Sql.string_t > Sql.t
  method url : < get : unit; nul : Sql.nullable; t : Sql.string_t > Sql.t
  method parent : < get : unit; nul : Sql.nullable; t : Sql.int32_t > Sql.t
end

type feed = {
  id : int32;
  url : string option;
  description : string;
  date : Calendar.t;
  author : int32;
  parent : int32 option;
  tags: string list
}

let feed_new data tags = {
  id = data#!id;
  url = data#?url;
  description = data#!description;
  date = data#!timedate;
  author = data#!author;
  parent = data#?parent;
  tags = tags
}

let links_of_tags tags =
  List.fold_left (fun acc tag ->
    let link = Html.a ~a: [Html.a_class ["tags"]] ~service: Services.tag_feed [Html.pcdata tag] (None, tag) in
    acc @ [Html.pcdata " "; link]
  ) [] tags

let to_html self =
  let content = match self.url with
    | Some l -> let url_service =
                  Eliom_service.external_service
                    l []
                    Eliom_parameter.unit () in
                Html.a ~a: [Html.a_class ["postitle"]] ~service: url_service
                  [Html.pcdata self.description] ()
    | None -> Html.pcdata self.description in
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
       content;
       Html.br ();
       Html.pcdata ("Publié le " ^ (Utils.string_of_calendar self.date) ^ " par ");
       Html.a Services.author_feed [Html.pcdata (author#!name)] (None, author#!name);
      ];
      [Html.br ();
       (* TODO : afficher "n commentaire(s)" *)
       Html.a
         Services.view_feed
         [Html.pcdata "n commentaires "]
         (* it's trap *)
         (Int32.to_int self.id, Utils.url_of_title self.description);
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
  Lwt.return (match self.url with
    | Some l -> Atom_feed.entry
                  ~updated: self.date
                  ~id: (Html.Xml.uri_of_string l)
                  ~title: (Atom_feed.plain self.description)
                [Atom_feed.authors [Atom_feed.author author#!name]]
    | None -> Atom_feed.entry
                  ~updated: self.date
                  ~id: (Html.Xml.uri_of_string (Int32.to_string self.id)) (* OLOL *)
                  ~title: (Atom_feed.plain self.description)
                [Atom_feed.authors [Atom_feed.author author#!name]]
  )

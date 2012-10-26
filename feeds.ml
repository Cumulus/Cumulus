module Calendar = CalendarLib.Calendar

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

let feeds_of_db feeds =
  let feeds =
    List.fold_right (fun feed acc ->
      (Feed.feed_new (feed :> Feed.feed_db)
         (List.fold_right
            (fun elm acc -> (elm#!tag) :: acc)
            (List.filter (fun elm -> elm#!id = feed#!id) feeds) []
         )
      ) :: acc
    ) feeds [] in
  Lwt.return (
    List.fold_right (fun feed acc ->
      if List.mem feed acc then
        acc
      else
        feed :: acc
    ) feeds []
  )

(* WARNING: le traitement présent dans feeds1 est la cause à l'affichage de
  * moins de 10 liens sur la page d'accueil. J'ai faim, pas envie de traiter le
  * bug mais il est là. En tout cas, pourquoi il y a se traitement, pourquoi il
  * y a des doublons dans les feeds (car apparament, c'est le cas), comment
  * éviter les doublons tout en affichant 10 liens ? *)

let to_somthing f data =
  Lwt_list.map_p (fun feed -> f feed) data

let private_to_html data =
  to_somthing
    (fun feed ->
      Feed.to_html feed >>= (fun elm ->
        Lwt.return (Html.div ~a: [Html.a_class ["line post"]] elm)
      )
    ) data

let author_to_html ~starting author =
  Db.get_feeds_with_author ~starting author
  >>= feeds_of_db
  >>= private_to_html

let tag_to_html ~starting tag =
  Db.get_feeds_with_tag ~starting tag
  >>= feeds_of_db
  >>= private_to_html

let to_html ~starting () =
  Db.get_feeds ~starting ()
  >>= feeds_of_db
  >>= private_to_html

let feed_id_to_html id =
  Db.get_feed_with_id id
  >>= feeds_of_db
  >>= private_to_html

(* FIXME? should atom feed return only a limited number of links ? *)
let to_atom () =
  Db.get_feeds ~number:100l ()
  >>= feeds_of_db
  >>= to_somthing Feed.to_atom
  >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id: (Html.Xml.uri_of_string "http://cumulus.org")
        ~title: (Atom_feed.plain "An Atom flux")
        tmp
    )
  )

let bus = Eliom_bus.create Json.t<unit>

let append_feed (url, (title, tags)) =
  User.get_userid () >>= fun userid ->
  match userid with
    | None -> Lwt.return Not_connected
    | (Some author) ->
      if (Utils.string_is_empty title || Utils.string_is_empty tags) then
        Lwt.return Empty
      else if Utils.is_invalid_url url then
        Lwt.return Invalid_url
      else
        Db.get_feed_url_with_url url >>= function
          | (Some _) -> Lwt.return Already_exist
          | None ->
            Db.add_feed
              url
              title
              (List.map (fun x -> String.lowercase (Utils.strip x)) (Str.split (Str.regexp "[,]+") tags))
              author >>= fun () ->
            Eliom_bus.write bus ();
            Lwt.return Ok

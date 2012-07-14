module Calendar = CalendarLib.Calendar

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

let feeds_of_db db =
  Lwt_list.map_p (fun elm -> Lwt.return (Feed.feed_new elm)) db

let get_all () =
  Db.get_feeds () >>= feeds_of_db

let get_with_author author =
  Db.get_feeds_with_author author >>= feeds_of_db

let to_somthing f data =
  Lwt_list.map_p
    (fun feed -> f feed)
    data

let private_to_html data =
  to_somthing
    (fun feed ->
      Feed.to_html feed >>= (fun elm ->
        Lwt.return (Html.p elm)
      )
    ) data

let author_to_html username =
  get_with_author username >>= private_to_html

let to_html () =
  get_all () >>= private_to_html

let to_atom () =
  get_all () >>= (to_somthing Feed.to_atom) >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id: (Html.Xml.uri_of_string "http://cumulus.org")
        ~title: (Atom_feed.plain "An Atom flux")
        tmp
    )
  )

let append_feed (url, (title, tags)) =
  User.get_userid () >>= (fun userid ->
    match userid with
      | None -> Lwt.return Not_connected
      | (Some author) ->
        if Utils.string_is_empty title then
          Lwt.return Empty
        else if Utils.is_invalid_url url then
          Lwt.return Invalid_url
        else (
          Db.get_feed_url_with_url url >>= (function
            | (Some _) -> Lwt.return Already_exist
            | None -> Db.add_feed url title tags author >>= (fun () ->
              Lwt.return Ok
            )
          )
        )
  )

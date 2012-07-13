module Calendar = CalendarLib.Calendar

type append_state = Ok | Not_connected | Empty | Already_exist | Invalid_url

let self =
  Ocsipersist.open_table "feeds"

let get_filter cmp =
  Ocsipersist.fold_table (fun url data ret ->
    Lwt.return (
      let feed = Feed.feed_new url data in
      if cmp feed then
        ret @ [feed]
      else
        ret
    )
  ) self []

let get_all () =
  get_filter (fun _ -> true)

let to_somthing f data =
  Lwt_list.map_p
    (fun feed -> Lwt.return (f feed))
    data

let private_to_html data =
  to_somthing (fun feed -> Html.p (Feed.to_html feed)) data

let author_to_html username =
  get_filter (Feed.filter_author username) >>= private_to_html

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
  User.get_username () >>= (fun username ->
    match username with
      | None -> Lwt.return Not_connected
      | (Some author) ->
        if Utils.string_is_empty title then
          Lwt.return Empty
        else if Utils.is_invalid_url url then
          Lwt.return Invalid_url
        else (
          let feed = Feed.feed_new_from_new url title author
                      (Str.split (Str.regexp "[ \t]+") tags) in
          Lwt.try_bind
            (fun () -> Ocsipersist.find self url)
            (fun _ -> Lwt.return Already_exist)
            (fun _ ->
              Feed.write feed (Ocsipersist.add self) >>= (fun () ->
                Lwt.return Ok
              )
            )
        )
  )

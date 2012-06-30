module Calendar = CalendarLib.Calendar
module Xml = Eliom_content_core.Xml

type append_state = Ok | Not_connected | Empty | Already_exist

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

let to_something entity getter =
  getter () >>= (fun self ->
    let content tmp feed = tmp @ [entity feed] in
    let rec f tmp = function
      | [] -> Lwt.return tmp
      | [x] -> f (content tmp x) []
      | x::xs -> f (content tmp x) xs in
    f [] self
  )

let author_to_html username =
  to_something
    (fun feed -> Html.p (Feed.to_html feed))
    (fun () -> get_filter (Feed.filter_author username))

let to_html () =
  to_something (fun feed ->
    Html.p (Feed.to_html feed)
  ) get_all

let to_atom () =
  to_something Feed.to_atom get_all >>= (fun tmp ->
    Lwt.return (
      Atom_feed.feed
        ~updated: (Calendar.make 2012 6 9 17 40 30)
        ~id: (Xml.uri_of_string "http://cumulus.org")
        ~title: (Atom_feed.plain "An Atom flux")
        tmp
    )
  )

let append_feed (url, (title, tags)) =
  User.get_username () >>= (fun username ->
    match username with
      | None -> Lwt.return Not_connected
      | (Some author) ->
        if Utils.string_is_empty url || Utils.string_is_empty title then
          Lwt.return Empty
        else
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

module Calendar = CalendarLib.Calendar
module Xml = Eliom_content_core.Xml

type feeds = Feed.feed_content Ocsipersist.table

let feeds_new () =
  Ocsipersist.open_table "feeds"

let get_all self =
  Ocsipersist.fold_table (fun url data ret ->
    Lwt.return (ret @ [Feed.feed_new url data])
  ) self []

let to_something self shell entity =
  get_all self >>= (fun self ->
    let content tmp feed = tmp @ [entity feed] in
    let rec f tmp = function
      | [] -> Lwt.return (shell tmp)
      | [x] -> f (content tmp x) []
      | x::xs -> f (content tmp x) xs in
    f [] self
  )

let to_html self =
  to_something self
    (fun tmp ->
      Html.html
        (Html.head (Html.title (Html.pcdata "Hello World")) [])
        (Html.body tmp)
    )
    (fun feed -> Html.p (Feed.to_html feed))

let to_atom self =
  to_something self
    (Atom_feed.feed
       ~updated: (Calendar.make 2012 6 9 17 40 30)
       ~id: (Xml.uri_of_string "http://cumulus.org")
       ~title: (Atom_feed.plain "An Atom flux")
    )
    Feed.to_atom

let append_feed self (url, title, author) =
  let feed = Feed.feed_new_from_new url title author in
  Lwt.try_bind
    (fun () -> Ocsipersist.find self url)
    (fun _ -> Lwt.return false)
    (fun _ ->
      Feed.write feed (Ocsipersist.add self) >>= (fun () ->
        Lwt.return true
      )
    )

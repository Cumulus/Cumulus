module Html = Eliom_content.Html5
module Xml = Eliom_content_core.Xml
module Calendar = CalendarLib.Calendar

let atom_service =
  Eliom_atom.Reg.register_service
    ~path: ["atom"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        (Atom_feed.feed
           ~updated: (Calendar.make 2012 6 9 17 40 30)
           ~id: (Xml.uri_of_string "http://cumulus.org")
           ~title: (Atom_feed.plain "An Atom flux")
           []
        )
    )

let init_test_service =
  Eliom_registration.Html5.register_service
    ~path: ["init_test"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let date = Calendar.now ()
      and table = Ocsipersist.open_table "feeds" in
      Ocsipersist.add table "http://url" ("TEST !", date, "Me") >>= (fun () ->
        Lwt.return
          (Html.D.html
             (Html.D.head (Html.D.title (Html.D.pcdata "Hello World")) [])
             (Html.D.body [])
          ))
    )

let string_of_calendar cal =
  (string_of_int (Calendar.day_of_month cal)) ^ "/" ^
    (string_of_int (Calendar.Date.int_of_month (Calendar.month cal))) ^ "/" ^
    (string_of_int (Calendar.year cal)) ^ " à " ^
    (string_of_int (Calendar.hour cal)) ^ ":" ^
    (string_of_int (Calendar.minute cal)) ^ ":" ^
    (string_of_int (Calendar.second cal))

let test_service =
  Eliom_registration.Html5.register_service
    ~path: ["test"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      Feeds.feeds_new () >>= (fun feeds ->
        let currify f tmp x = f (tmp @ [Html.D.p [
          Html.D.pcdata ("url: " ^ x.Feed.url);
          Html.D.br ();
          Html.D.pcdata ("title: " ^ x.Feed.title);
          Html.D.br ();
          Html.D.pcdata ("date: " ^ (string_of_calendar x.Feed.date));
          Html.D.br ();
          Html.D.pcdata ("author: " ^ x.Feed.author)
        ]]) in
        let rec f tmp = function
          | [] ->
            Lwt.return
              (Html.D.html
                 (Html.D.head (Html.D.title (Html.D.pcdata "Hello World")) [])
                 (Html.D.body tmp)
              )
          | [x] -> currify f tmp x []
          | x::xs -> currify f tmp x xs in
        f [] feeds)
    )

let main_service =
  Eliom_registration.Html5.register_service
    ~path: [""]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        (Html.D.html
           (Html.D.head (Html.D.title (Html.D.pcdata "Hello World")) [])
           (Html.D.body [
             Html.D.p [
               Html.D.pcdata "Hello World"
             ]
           ])
        )
    )

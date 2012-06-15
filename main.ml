module Html = Eliom_content.Html5.D
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

let init_test_service_with_string =
  Eliom_registration.Html5.register_service
    ~path: ["init_test"]
    ~get_params: (Eliom_parameter.string "url")
    (fun url () ->
      let date = Calendar.now ()
      and table = Ocsipersist.open_table "feeds" in
      Ocsipersist.add table url ("TEST !", date, "Me") >>= (fun () ->
        Lwt.return
          (Html.html
             (Html.head (Html.title (Html.pcdata "Hello World")) [])
             (Html.body [])
          )
      )
    )

let init_test_service =
  Eliom_registration.Html5.register_service
    ~path: ["init_test"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Hello World")) [])
           (Html.body [
             Html.get_form
               init_test_service_with_string
               (fun string_name -> [
                 Html.p [
                   Html.string_input ~input_type: `Text ~name: string_name ();
                   Html.string_input ~input_type: `Submit ~value: "Send" ()
                 ]])
           ])
        )
    )

let test_service =
  Eliom_registration.Html5.register_service
    ~path: ["test"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let feeds = Feeds.feeds_new () in
      Feeds.to_html feeds
    )

let main_service =
  Eliom_registration.Html5.register_service
    ~path: [""]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Hello World")) [])
           (Html.body [
             Html.p [
               Html.pcdata "Hello World"
             ]
           ])
        )
    )

let atom_service =
  Eliom_atom.Reg.register_service
    ~path: ["atom"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let feeds = Feeds.feeds_new () in
      Feeds.to_atom feeds
    )

let init_test_service_with_string =
  Eliom_registration.Html5.register_service
    ~path: ["init_test"]
    ~get_params: Eliom_parameter.((string "url") ** (string "title"))
    (fun (url, title) () ->
      User.get_username () >>= (fun username ->
        match username with
          | None ->
            Lwt.return
              (Html.html
                 (Html.head (Html.title (Html.pcdata "Hello World")) [])
                 (Html.body [
                   Html.p [
                     Html.pcdata "Vous ne vous etes pas autentifie"
                   ]
                 ])
              )
          | (Some author) ->
            let feeds = Feeds.feeds_new () in
            Feeds.append_feed feeds (url, title, author) >>= (fun state ->
              Lwt.return
                (Html.html
                   (Html.head (Html.title (Html.pcdata "Hello World")) [])
                   (Html.body [Html.p [Html.pcdata (string_of_bool state)]])
                )
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
               (fun (url_name, title_name) -> [
                 Html.p [
                   Html.string_input ~input_type: `Text ~name: url_name ();
                   Html.string_input ~input_type: `Text ~name: title_name ();
                   Html.string_input ~input_type: `Submit ~value: "Send" ()
                 ]
               ])
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

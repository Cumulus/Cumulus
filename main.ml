let atom_service =
  Eliom_atom.Reg.register_service
    ~path: ["atom.xml"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let feeds = Feeds.feeds_new () in
      Feeds.to_atom feeds
    )

let rec main_service_with_string_fun () =
  Eliom_registration.Html5.register_service
    ~path: [""]
    ~get_params: Eliom_parameter.((string "url") ** (string "title"))
    (fun (url, title) () ->
      User.get_username () >>= (fun username ->
        let feeds = Feeds.feeds_new () in
        Templates.main feeds
          (match username with
            | None -> Lwt.return [
              Html.p [
                Html.pcdata "Vous ne vous etes pas autentifie"
              ]
            ]
            | (Some author) ->
              let feeds = Feeds.feeds_new () in
              Feeds.append_feed feeds (url, title, author) >>=
                (fun state -> Lwt.return [
                  Html.p [
                    Html.pcdata (string_of_bool state)
                  ]
                ])
          )
          (main_service_with_string_fun ())
      )
    )

let main_service_with_string = main_service_with_string_fun ()

let main_service =
  Eliom_registration.Html5.register_service
    ~path: [""]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let feeds = Feeds.feeds_new () in
      Templates.main feeds (Lwt.return []) main_service_with_string
    )

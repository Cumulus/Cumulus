let atom_service_reg =
  Eliom_atom.Reg.register_service
    ~path: ["atom.xml"]
    ~get_params: Eliom_parameter.unit
    (fun () () ->
      let feeds = Feeds.feeds_new () in
      Feeds.to_atom feeds
    )

let main_service =
  Eliom_service.service
    ~path: [""]
    ~get_params: Eliom_parameter.unit
    ()

let append_feed_service =
  Eliom_service.service
    ~path: [""]
    ~get_params: Eliom_parameter.((string "url") ** (string "title"))
    ()

let auth_service =
  (* post_coservice' *)
  Eliom_service.post_service
    ~fallback: main_service
    ~post_params: Eliom_parameter.((string "username") ** (string "password"))
    ()

let add_user_service =
  Eliom_service.post_service
    ~fallback: main_service
    ~post_params: Eliom_parameter.((string "username") **
                                      (string "password") **
                                      (string "email"))
    ()

let registration_service =
  Eliom_service.service
    ~path: ["registration"]
    ~get_params: Eliom_parameter.unit
    ()

let regs =
  Eliom_registration.Html5.register
    ~service: main_service
    (fun () () ->
      let feeds = Feeds.feeds_new () in
      Templates.main feeds (Lwt.return [])
        append_feed_service
        auth_service
        registration_service
    );
  Eliom_registration.Html5.register
    ~service: append_feed_service
    (fun (url, title) () ->
      User.get_username () >>= (fun username ->
        let feeds = Feeds.feeds_new () in
        Templates.main feeds
          (match username with
            | None -> Utils.msg "Vous ne vous etes pas autentifie"
            | (Some author) ->
              if Utils.string_is_empty url || Utils.string_is_empty title then
                Utils.msg "L'un des champs est vide"
              else
                Feeds.append_feed feeds (url, title, author) >>=
                  (fun state -> Utils.msg (
                    if state then
                      "Le lien a bien ete ajoute"
                    else
                      "Le lien existe deja"
                   ))
          )
          append_feed_service
          auth_service
          registration_service
      )
    );
  Eliom_registration.Html5.register
    ~service: auth_service
    (fun () (username, password) ->
      let users = Users.users_new ()
      and feeds = Feeds.feeds_new () in
      Templates.main feeds
        (Users.connect_user users username password >>= (fun state ->
          match state with
            | User.Already_connected -> Utils.msg "Deja connecte"
            | User.Ok -> Utils.msg "Connecte"
            | User.Bad_password -> Utils.msg "Mauvais mot-de-passe"
            | User.Not_found -> Utils.msg "Utilisateur non trouve"
         )
        )
        append_feed_service
        auth_service
        registration_service
    );
  Eliom_registration.Html5.register
    ~service: add_user_service
    (fun () (username, (password, email)) ->
      let users = Users.users_new ()
      and feeds = Feeds.feeds_new () in
      Templates.main feeds
        (* Add an user *)
        (Lwt.return [])
        append_feed_service
        auth_service
        registration_service
    );
  Eliom_registration.Html5.register
    ~service: registration_service
    (fun () () ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Cumulus")) [])
           (Html.body [
             Html.post_form add_user_service
               (fun (url_name, (title_name, email_name)) -> [
                 Html.p [
                   Html.string_input
                     ~input_type: `Text
                     ~name: url_name ();
                   Html.string_input
                     ~input_type: `Text
                     ~name: title_name ();
                   Html.string_input
                     ~input_type: `Text
                     ~name: email_name ();
                   Html.string_input
                     ~input_type: `Submit
                     ~value: "Send" ()
                 ]
               ]) ()
           ])
        )
    )

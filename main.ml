let atom_service_reg =
  Eliom_atom.Reg.register_service
    ~path: ["atom.xml"]
    ~get_params: Eliom_parameter.unit
    (fun () () -> Feeds.to_atom ())

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

let apply_services f =
  f append_feed_service
    auth_service
    registration_service

let regs =
  Eliom_registration.Html5.register
    ~service: main_service
    (fun () () ->
      apply_services (Templates.main [])
    );
  Eliom_registration.Html5.register
    ~service: append_feed_service
    (fun data () ->
      Feeds.append_feed data >>= (fun state ->
        apply_services
          (Templates.main
             (Utils.msg (match state with
               | Feeds.Not_connected -> "Vous ne vous etes pas autentifie"
               | Feeds.Empty -> "L'un des champs est vide"
               | Feeds.Already_exist -> "Le lien existe deja"
               | Feeds.Ok -> "Le lien a bien ete ajoute"
              ))
          )
      )
    );
  Eliom_registration.Html5.register
    ~service: auth_service
    (fun () (username, password) ->
      Users.connect_user username password >>= (fun state ->
        apply_services
          (Templates.main
            (Utils.msg (match state with
              | User.Already_connected -> "Deja connecte"
              | User.Ok -> "Connecte"
              | User.Bad_password -> "Mauvais mot-de-passe"
              | User.Not_found -> "Utilisateur non trouve"
             ))
          )
      )
    );
  Eliom_registration.Html5.register
    ~service: add_user_service
    (fun () (username, (password, email)) ->
      (Users.add_user username password email >>= (fun state ->
        apply_services
          (Templates.main
             (Utils.msg (match state with
               | true ->"Vous etes bien enregistre"
               | false -> "L'user exist deja"
              ))
          )
         ))
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

let _ =
  Eliom_atom.Reg.register
    ~service: Services.atom
    (fun () () -> Feeds.to_atom ());
  Eliom_registration.Html5.register
    ~service: Services.main
    (fun () () -> Templates.main []);
  Eliom_registration.Html5.register
    ~service: Services.author_feed
    (fun username () -> Templates.user [] username);
  Eliom_registration.Html5.register
    ~service: Services.append_feed
    (fun () data ->
      Feeds.append_feed data >>= (fun state ->
        Templates.main
          (Utils.msg (match state with
            | Feeds.Not_connected -> "Vous ne vous etes pas autentifie"
            | Feeds.Empty -> "L'un des champs est vide"
            | Feeds.Already_exist -> "Le lien existe deja"
            | Feeds.Ok -> "Le lien a bien ete ajoute"
           ))
      )
    );
  Eliom_registration.Html5.register
    ~service: Services.auth
    (fun () (username, password) ->
      Users.connect_user username password >>= (fun state ->
        Templates.main
          (Utils.msg (match state with
            | User.Already_connected -> "Deja connecte"
            | User.Ok -> "Connecte"
            | User.Bad_password -> "Mauvais mot-de-passe"
            | User.Not_found -> "Utilisateur non trouve"
           ))
      )
    );
  Eliom_registration.Html5.register
    ~service: Services.add_user
    (fun () datas ->
      Users.add_user datas >>= (fun state ->
        Templates.main
          (Utils.msg (match state with
            | true -> "Vous etes bien enregistre"
            | false -> "L'user exist deja"
           ))
      )
    );
  Eliom_registration.Html5.register
    ~service: Services.registration
    (fun () () ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Cumulus")) [])
           (Html.body [
             Html.post_form Services.add_user
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
    );
  ()

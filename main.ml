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
            | Feeds.Invalid_url -> "L'Url entrée est invalide."
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
            | true -> "Vous êtes bien enregistré"
            | false -> "L'user existe deja, ou mot de passe invalide"
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
               (fun (username_name, (email_name, (password_name,
               password_check))) -> [
                 Html.p [
                   Html.pcdata "Nom d'utilisateur: ";
                   Html.string_input
                     ~input_type: `Text
                     ~name: username_name ();
                   Html.br ();
                   Html.pcdata "Mot de passe: ";
                   Html.string_input
                     ~input_type: `Password
                     ~name: password_name ();
                   Html.pcdata "Mot de passe: ";
                   Html.string_input
                     ~input_type: `Password
                     ~name: password_check ();
                   Html.br ();
                   Html.pcdata "Email: ";
                   Html.string_input
                     ~input_type: `Text
                     ~name: email_name ();
                  Html.br ();
                   Html.string_input
                     ~input_type: `Submit
                     ~value: "Send" ()
                 ]
               ]) ()
           ])
        )
    );
  ()

let () =
  Eliom_atom.Reg.register
    ~service: Services.atom
    (fun () () -> Feeds.to_atom ());
  Eliom_registration.Html5.register
    ~service: Services.main
    (fun page () -> Templates.main ?page []);
  Eliom_registration.Html5.register
    ~service: Services.author_feed
    (fun (page, username) () -> Templates.user ?page [] username);
  Eliom_registration.Html5.register
    ~service: Services.append_feed
    (fun page data ->
      Feeds.append_feed data >>= (fun state ->
        Templates.main ?page
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
    (fun page (username, password) ->
      Users.connect_user username password >>= (fun state ->
        Templates.main ?page
          (Utils.msg (match state with
            | User.Already_connected -> "Deja connecte"
            | User.Ok -> "Connecte"
            | User.Bad_password -> "Mauvais mot-de-passe"
            | User.Not_found -> "Utilisateur non trouve"
           ))
      )
    );
  Eliom_registration.Html5.register
    ~service: Services.view_feed
    (fun (id, name) () ->
      Templates.view_feed id);
  Eliom_registration.Html5.register
    ~service: Services.add_user
    (fun page datas ->
      Users.add_user datas >>= (fun state ->
        Templates.main ?page
          (Utils.msg (match state with
            | true -> "Vous êtes bien enregistré"
            | false -> "L'user existe deja, ou mot de passe invalide"
           ))
      )
    );
  Eliom_registration.Html5.register
    ~service: Services.registration
    (fun () () -> Templates.register);
  Eliom_registration.Html5.register
    ~service: Services.tag_feed
    (fun (page, tag) () -> Templates.tag ?page [] tag)

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
      Feeds.append_feed data >>= fun state ->
      Templates.main ?page
        (Utils.msg (match state with
          | Feeds.Not_connected -> "Vous ne vous etes pas autentifie"
          | Feeds.Empty -> "L'un des champs est vide"
          | Feeds.Invalid_url -> "L'Url entrée est invalide."
          | Feeds.Already_exist -> "Le lien existe deja"
          | Feeds.Ok -> "Le lien a bien ete ajoute"
         ))
    );
  Eliom_registration.Action.register
    ~service: Services.auth
    (fun () (username, password) ->
      Users.connect_user username password >>= User.set_login_state_from_user_state
    );
  Eliom_registration.Action.register
    ~service: Services.disconnect
    (fun () () ->
      User.disconnect () >>= User.set_login_state_from_disconnect
    );
  Eliom_registration.Html5.register
    ~service: Services.view_feed
    (fun (id, name) () ->
      Templates.view_feed id
    );
  Eliom_registration.Html5.register
    ~service: Services.add_user
    (fun page datas ->
      Users.add_user datas >>= fun state ->
      Templates.main ?page
        (Utils.msg (match state with
          | true -> "Vous êtes bien enregistré"
          | false -> "L'utilisateur existe deja, ou le mot de passe est invalide"
         ))
    );
  Eliom_registration.Html5.register
    ~service: Services.update_user_mail
    (fun page datas ->
      Users.update_user_mail datas >>= fun state ->
      Templates.preferences
        (Utils.msg (match state with
          | true -> "Modification de l'adresse mail effectuée"
          | false -> "Adresse invalide"
         ))
    );
  Eliom_registration.Html5.register
    ~service: Services.update_user_password
    (fun page datas ->
      Users.update_user_password datas >>= fun state ->
      Templates.preferences
        (Utils.msg (match state with
          | true -> "Mot de passe changé."
          | false -> "Les mots de passe sont invalides ou ne correspondent pas."
         ))
    );
  Eliom_registration.Html5.register
    ~service: Services.registration
    (fun () () -> Templates.register ());
  Eliom_registration.Html5.register
    ~service: Services.tag_feed
    (fun (page, tag) () -> Templates.tag ?page [] tag);
  Eliom_registration.Html5.register
    ~service: Services.preferences
    (fun () () -> Templates.preferences []);
  Eliom_registration.Action.register
    ~service:Services.delete_feed
    (fun feed () ->
      User.get_userid () >>= function
        | None -> Lwt.return ()
        | Some userid -> Db.delete_feed feed userid
    )

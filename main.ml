module Cumulus_appl = Eliom_registration.App (struct
  let application_name = "cumulus"
end)

let () =
  Eliom_atom.Reg.register
    ~service: Services.atom
    (fun () () -> Feeds.to_atom ());
  Cumulus_appl.register
    ~service: Services.main
    (fun page () -> Templates.main ?page []);
  Cumulus_appl.register
    ~service: Services.author_feed
    (fun (page, username) () -> Templates.user ?page [] username);
  Cumulus_appl.register
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
  Cumulus_appl.register
    ~service: Services.view_feed
    (fun (id, name) () ->
      Templates.view_feed id
    );
  Cumulus_appl.register
    ~service: Services.add_user
    (fun page datas ->
      Users.add_user datas >>= fun state ->
      Templates.main ?page
        (Utils.msg (match state with
          | true -> "Vous êtes bien enregistré"
          | false -> "L'utilisateur existe deja, ou le mot de passe est invalide"
         ))
    );
  Cumulus_appl.register
    ~service: Services.update_user_mail
    (fun page datas ->
      Users.update_user_mail datas >>= fun state ->
      Templates.preferences
        (Utils.msg (match state with
          | true -> "Modification de l'adresse mail effectuée"
          | false -> "Adresse invalide"
         ))
    );
  Cumulus_appl.register
    ~service: Services.update_user_password
    (fun page datas ->
      Users.update_user_password datas >>= fun state ->
      Templates.preferences
        (Utils.msg (match state with
          | true -> "Mot de passe changé."
          | false -> "Les mots de passe sont invalides ou ne correspondent pas."
         ))
    );
  Cumulus_appl.register
    ~service: Services.registration
    (fun () () -> Templates.register ());
  Cumulus_appl.register
    ~service: Services.tag_feed
    (fun (page, tag) () -> Templates.tag ?page [] tag);
  Cumulus_appl.register
    ~service: Services.preferences
    (fun () () -> Templates.preferences []);
  Eliom_registration.Action.register
    ~service:Services.delete_feed
    (fun feed () ->
      User.get_userid () >>= function
        | None -> Lwt.return ()
        | Some userid -> Db.delete_feed feed userid
    )

module Cumulus_appl = Eliom_registration.App (struct
  let application_name = "cumulus"
end)

let (>>=) = Lwt.(>>=)

let () =
  Eliom_atom.Reg.register
    ~service: Services.atom
    (fun () () -> Feeds.to_atom ());
  Cumulus_appl.register
    ~service: Services.main
    (fun page () ->
      let service =
        Eliom_service.preapply ~service:Services.main page
      in
      Templates.main ?page ~service ()
    );
  Cumulus_appl.register
    ~service: Services.author_feed
    (fun (page, username) () ->
      let service =
        Eliom_service.preapply ~service:Services.author_feed (page, username)
      in
      Templates.user ?page ~service username
    );
  Eliom_registration.Ocaml.register
    ~service:Services.append_feed
    (fun () data ->
      Feeds.append_feed data >>= function
        | Feeds.Not_connected -> Lwt.return "Vous ne vous etes pas autentifie"
        | Feeds.Empty -> Lwt.return "L'un des champs est vide"
        | Feeds.Invalid_url -> Lwt.return "L'Url entrée est invalide."
        | Feeds.Already_exist -> Lwt.return "Le lien existe deja"
        | Feeds.Ok -> Lwt.return "Le lien a bien ete ajoute"
    );
  Eliom_registration.Action.register
    ~service: Services.auth
    (fun () (username, password) ->
      Users.connect_user username password
      >>= User.set_login_state_from_user_state
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
  Eliom_registration.Ocaml.register
    ~service:Services.add_user
    (fun () data ->
      Users.add_user data >>= function
        | true -> Lwt.return "Vous êtes bien enregistré"
        | false ->
            Lwt.return
              "L'utilisateur existe deja, ou le mot de passe est invalide"
    );
  Eliom_registration.Ocaml.register
    ~service:Services.update_user_mail
    (fun () data ->
      Users.update_user_mail data >>= function
        | true -> Lwt.return "Modification de l'adresse mail effectuée"
        | false -> Lwt.return "Adresse invalide"
    );
  Eliom_registration.Ocaml.register
    ~service:Services.update_user_password
    (fun () data ->
      Users.update_user_password data >>= function
        | true -> Lwt.return "Mot de passe changé."
        | false ->
            Lwt.return
              "Les mots de passe sont invalides ou ne correspondent pas."
    );
  Cumulus_appl.register
    ~service: Services.registration
    (fun () () -> Templates.register ());
  Cumulus_appl.register
    ~service: Services.tag_feed
    (fun (page, tag) () ->
      let service =
        Eliom_service.preapply ~service:Services.tag_feed (page, tag)
      in
      Templates.tag ?page ~service tag
    );
  Cumulus_appl.register
    ~service: Services.preferences
    (fun () () -> Templates.preferences ());
  Eliom_registration.Action.register
    ~service:Services.delete_feed
    (fun feed () ->
      User.get_userid () >>= function
        | None -> Lwt.return ()
        | Some userid -> Db.delete_feed feed userid
    )

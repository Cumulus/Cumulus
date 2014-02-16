(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Batteries
open Eliom_lib.Lwt_ops

module Cumulus_appl = Eliom_registration.App (struct
  let application_name = "cumulus"
end)

let update_feed id =
  Lwt.async
    (fun () ->
       Lwt_unix.sleep 1. >>= fun () ->
       User.get_user () >>= fun user ->
       Feed.get_feed_with_id ~user:(User.get_id user) id >|= fun feed ->
       let content = Templates_feeds.feed_to_html_aux ~user feed in
       Client.call_event (id, Client.Feed content);
    );
  Feeds.Ok

let update_vote feedid () =
  Lwt.async
    (fun () ->
       Lwt_unix.sleep 1. >>= fun () ->
       Feed.get_score_from_id ~feedid >|= fun score ->
       Client.call_event (feedid, Client.Score score)
    )

let () =
  Eliom_atom.Reg.register
    ~service: Services.atom
    (fun () () -> Templates_atom.to_atom ());
  Eliom_atom.Reg.register
    ~service: Services.comments_atom
    (fun () () -> Templates_atom.comments_to_atom ());
  Eliom_atom.Reg.register
    ~service: Services.atom_feed
    (fun (feed_id) () -> Templates_atom.tree_to_atom feed_id ());
  Eliom_atom.Reg.register
    ~service: Services.atom_tag
    (fun (tag) () -> Templates_atom.tag_to_atom tag ());
  Cumulus_appl.register
    ~service: Services.main
    (fun () -> Templates.main);
  Cumulus_appl.register
    ~service: Services.author_feed
    (fun username () -> Templates.user username);
  Cumulus_appl.register
    ~service: Services.fav_feed
    (fun username () -> Templates.fav_feed username);
  Eliom_registration.Action.register
    ~service:Services.append_feed
    (fun () data ->
       Feeds.append_feed ~update:update_feed data >>= (function
         | Feeds.Not_connected -> Lwt.return "Vous ne vous êtes pas authentifié"
         | Feeds.Empty -> Lwt.return "L'un des champs est vide"
         | Feeds.Invalid_url -> Lwt.return "L'Url entrée est invalide"
         | Feeds.Already_exist -> Lwt.return "Le lien existe déjà"
         | Feeds.Ok -> Lwt.return "Le lien a bien été ajouté"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.append_link_comment
    (fun (_, _) data ->
       Feeds.append_link_comment ~update:update_feed data >>= (function
         | Feeds.Not_connected -> Lwt.return "Vous ne vous êtes pas authentifié"
         | Feeds.Empty -> Lwt.return "L'un des champs est vide"
         | Feeds.Invalid_url -> Lwt.return "L'Url entrée est invalide"
         | Feeds.Already_exist -> Lwt.return "Le lien existe déjà"
         | Feeds.Ok -> Lwt.return "Le commentaire a bien été ajouté"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.append_desc_comment
    (fun (_, _) data ->
       Feeds.append_desc_comment ~update:update_feed data >>= (function
         | Feeds.Not_connected -> Lwt.return "Vous ne vous êtes pas authentifié"
         | Feeds.Empty -> Lwt.return "L'un des champs est vide"
         | Feeds.Invalid_url -> Lwt.return "L'Url entrée est invalide"
         | Feeds.Already_exist -> Lwt.return "Le lien existe déjà"
         | Feeds.Ok -> Lwt.return "Le lien a bien été ajouté"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.edit_link_comment
    (fun (_, _) data ->
       Feeds.edit_link_comment data >>= (function
         | Feeds.Not_connected -> Lwt.return "Vous ne vous êtes pas authentifié"
         | Feeds.Empty -> Lwt.return "L'un des champs est vide"
         | Feeds.Invalid_url -> Lwt.return "L'Url entrée est invalide"
         | Feeds.Already_exist -> Lwt.return "Le lien existe déjà"
         | Feeds.Ok -> Lwt.return "Édition réussie"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.edit_desc_comment
    (fun (_, _) data ->
       Feeds.edit_desc_comment data >>= (function
         | Feeds.Not_connected -> Lwt.return "Vous ne vous êtes pas authentifié"
         | Feeds.Empty -> Lwt.return "L'un des champs est vide"
         | Feeds.Invalid_url -> Lwt.return "L'Url entrée est invalide"
         | Feeds.Already_exist -> Lwt.return "Le lien existe déjà"
         | Feeds.Ok -> Lwt.return "Édition réussie"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service: Services.auth
    (fun () (username, password) ->
       User.connect username password >>= (function
         | User.Already_connected -> Lwt.return "Déjà connecté"
         | User.Ok -> Lwt.return "Connecté"
         | User.Bad_password -> Lwt.return "Mauvais mot de passe"
         | User.Not_found -> Lwt.return "Utilisateur non trouvé"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service: Services.disconnect
    (fun () () ->
       User.disconnect () >>= (function
         | true -> Lwt.return "Déconnecté"
         | false -> Lwt.return "Quoi la baise ?"
       )
       >>= Errors.set_error
    );
  Cumulus_appl.register
    ~service: Services.view_feed'
    (fun ((id, _), _) () ->
       Templates.view_feed id
    );
  Cumulus_appl.register
    ~service: Services.view_feed
    (fun (id, _) () ->
       Templates.view_feed id
    );
  Eliom_registration.Action.register
    ~service:Services.add_user
    (fun () data ->
       User.add data >>= (function
         | true -> Lwt.return "Vous êtes bien enregistré"
         | false ->
             Lwt.return
               "L'utilisateur existe deja, ou une information est invalide"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.update_user_mail
    (fun () data ->
       User.update_email data >>= (function
         | true -> Lwt.return "Modification de l'adresse mail effectuée"
         | false -> Lwt.return "Adresse invalide"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.update_user_feeds_per_page
    (fun () data ->
       User.update_feeds_per_page data >>= (function
         | true -> Lwt.return "Nombre de liens par page modifié"
         | false -> Lwt.return "Nombre invalide"
       )
       >>= Errors.set_error
    );
  Eliom_registration.Action.register
    ~service:Services.update_user_password
    (fun () data ->
       User.update_password data >>= (function
         | true -> Lwt.return "Mot de passe changé."
         | false ->
             Lwt.return
               "Les mots de passe sont invalides ou ne correspondent pas."
       )
       >>= Errors.set_error
    );
  Cumulus_appl.register
    ~service: Services.registration
    (fun () () -> Templates.register ());
  Cumulus_appl.register
    ~service: Services.tag_feed
    (fun tag () -> Templates.tag tag);
  Cumulus_appl.register
    ~service: Services.preferences
    (fun () () -> Templates.preferences ());
  Cumulus_appl.register
    ~service: Services.comment
    (fun (id, _) () -> Templates.comment id);
  Cumulus_appl.register
    ~service: Services.edit_feed
    (fun (id, _) () -> Templates.edit_feed id);
  Eliom_registration.Action.register
    ~service:Services.delete_feed
    (fun feedid () ->
       User.get_userid () >>= function
       | None -> Lwt.return ()
       | Some userid -> Feed.delete_feed_check ~feedid ~userid ()
    );
  Eliom_registration.Ocaml.register
    ~service:Services.add_fav_feed
    (fun feedid () -> Feed.add_fav feedid);
  Eliom_registration.Ocaml.register
    ~service:Services.del_fav_feed
    (fun feedid () -> Feed.del_fav feedid);
  Eliom_registration.Ocaml.register
    ~service:Services.upvote_feed
    (fun feedid () -> Feed.upvote ~update:(update_vote feedid) feedid);
  Eliom_registration.Ocaml.register
    ~service:Services.downvote_feed
    (fun feedid () -> Feed.downvote ~update:(update_vote feedid) feedid);
  Eliom_registration.Ocaml.register
    ~service:Services.cancelvote_feed
    (fun feedid () -> Feed.cancel_vote ~update:(update_vote feedid) feedid);
  Eliom_registration.Action.register
    ~service:Services.reset_password
    (fun () ->
       let service user =
         Eliom_registration.Action.register_coservice
           ~timeout:(float_of_int (60 * 60 * 24 * 30))
           ~fallback:Services.preferences
           ~get_params:Eliom_parameter.unit
           (fun () () -> User.force_connect user)
       in
       User.send_reset_email ~service
    );
  Cumulus_appl.register
    ~service:Services.reset_password_form
    (fun () () -> Templates.reset_password ())

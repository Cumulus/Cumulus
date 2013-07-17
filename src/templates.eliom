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

{shared{
  let (>>=) = Lwt.(>>=)
}}

{client{
  let display_error error_frame =
    let id_timeout = ref None in
    id_timeout := Some
      (Dom_html.window##setTimeout
         (Js.wrap_callback
            (fun () ->
              Eliom_content.Html5.Manip.removeAllChild error_frame;
              match !id_timeout with
                | None -> () (* It cannot happen *)
                | Some id ->
                    Dom_html.window##clearTimeout (id)
            ),
          5_000.
         )
      )

  (* Reloading feeds*)
  let () =
    let service = Eliom_service.void_coservice' in
    let event = %Feeds.event in
    let stream = Lwt_react.E.to_stream event in
    Lwt.async
      (fun () ->
        Lwt_stream.iter_s
          (fun () ->
            Eliom_client.change_page ~service () ()
          )
          stream
      )
}}

let string_input_box ?(a=[]) =
  Html.string_input ~a:(Html.a_class ["input-box"] :: a)

let submit_input ?(a=[]) =
  Html.string_input
    ~a:(Html.a_class ["btn-box"] :: a)
    ~input_type:`Submit

let user_form () =
  Lwt.return
    [ Html.div
        ~a: [Html.a_class ["mod";"right"]][
          Html.post_form
            ~a: [Html.a_class ["right"]]
            ~service: Services.auth
            (fun (user_name, password_name) -> [
              Html.string_input
                ~a: [Html.a_placeholder "Pseudo"]
                ~input_type: `Text
                ~name: user_name ();
              Html.string_input
                ~a: [Html.a_placeholder "Mot de passe"]
                ~input_type: `Password
                ~name: password_name ();
              Html.string_input
                ~input_type: `Submit
                ~value: "Connexion" ();
              Html.a
                ~a: [Html.a_class ["nav"]]
                ~service: Services.registration
                [Html.pcdata "Inscription"] ();
             ]
            )
            ()
        ]
    ]

let user_information user =
  Lwt.return
    [ Html.div
        ~a:[Html.a_class ["mod";"right"]]
        [ Html.post_form
            ~a:[Html.a_class ["right"]]
            ~service:Services.disconnect
            (fun () ->
              [ Html.p
                  [ Html.a
                      ~a:[Html.a_class ["nav"]]
                      ~service:Services.fav_feed
                      [Html.pcdata "Favoris"]
                      (Sql.get user#name, Some 0);
                    Html.a
                      ~a:[Html.a_class ["nav"]]
                      ~service:Services.preferences
                      [Html.pcdata "Préférences"]
                      ();
                    Html.string_input
                      ~input_type:`Submit
                      ~value:"Déconnexion"
                      ();
                    Html.img
                      ~alt:(Sql.get user#name)
                      ~src:(
                        Html.make_uri
                          ~service: (Utils.get_gravatar (Sql.get user#email))
                          (30, "identicon")
                      )
                      ();
                  ]
              ]
            )
            ()
        ]
    ]

let user_info () =
  User.get_user_and_email () >>= (function
    | Some user -> user_information user
    | None -> user_form ()
  )
  >>= fun content ->
  Lwt.return
    [ Html.header
        ~a:[Html.a_class ["line mod"]]
        ( [ Html.div
              ~a:[Html.a_class ["mod left"]]
              [ Html.a
                  ~a:[Html.a_class ["title"]]
                  ~service: Services.main
                  [Html.pcdata "Cumulus Project"]
                  None;
              ];
          ]
          @ content
        )
    ]

let main_style content footer =
  user_info () >>= fun user ->
  let base_error_frame =
    Eliom_content.Html5.D.div
      ~a:[Html.a_class ["msghandler"]]
  in
  Errors.get_error () >>= (function
    | Some error ->
        let error_frame =
          base_error_frame [Html.p [Html.pcdata error]]
        in
        ignore {unit{
          display_error %error_frame
        }};
        Lwt.return error_frame
    | None -> Lwt.return (base_error_frame [])
  )
  >>= fun error_frame ->
  Lwt.return
    (Html.html
       (Html.head
          (Html.title
             (Html.pcdata "Cumulus")
          )
          [ Html.css_link
              ~uri: (Html.make_uri
                       ~service: (Eliom_service.static_dir ())
                       ["knacss.css"]
              ) ();
            Html.css_link
              ~uri: (Html.make_uri
                       ~service: (Eliom_service.static_dir ())
                       ["cumulus.css"]
              ) ();
          ]
       )
       (Html.body
         [ Html.div
             ~a: [Html.a_class ["container"]]
             (user
              @ [ Html.div
                    ~a:[Html.a_class ["dash"]]
                    [ Html.post_form
                        ~service:Services.append_feed
                        (fun (url_name, (title_name, tags_name)) -> [
                          Html.string_input
                            ~a:[Html.a_placeholder "URL"]
                            ~input_type:`Text
                            ~name:url_name
                            ();
                          Html.string_input
                            ~a:[Html.a_placeholder "Titre"]
                            ~input_type:`Text
                            ~name:title_name
                            ();
                          Html.string_input
                            ~a:[Html.a_placeholder "Tags"]
                            ~input_type:`Text
                            ~name:tags_name
                            ();
                          Html.string_input
                            ~a:[Html.a_class ["btn btn-primary"]]
                            ~input_type:`Submit
                            ~value: "Envoyer !"
                            ()
                         ])
                        ()
                    ];
                  error_frame;
                ]
              @ content
              @ [Html.div ~a: [Html.a_class ["navigation"]]footer]
              @ [ Html.footer
                    ( [ Html.br ();
                        Html.br ();
                        Html.pcdata "(not so) Proudly propulsed by the inglorious ";
                        Html.Raw.a ~a:[Html.a_href
                                          (Html.uri_of_string
                                             (fun () ->
                                               "http://bitbucket.org/Engil/cumulus"
                                             )
                                          )
                                      ]
                          [Html.pcdata "Cumulus Project"];
                        Html.pcdata ", with love, and the ";
                        Html.Raw.a ~a:[Html.a_href
                                          (Html.uri_of_string
                                             (fun () -> "http://ocsigen.org/")
                                          )
                                      ]
                          [Html.pcdata "OCaml web framework Ocsigen"];
                        Html.a ~service:Services.atom
                          [Html.pcdata "    (Flux Atom du site)"] ();
                      ]
                    )
                ]
             )
         ]
       )
    )

let link_footer ~link min max = function
  | page when page = min && page < max -> [ link "Suivant" (Some (page + 1)) ]
  | page when page = max && page > min -> [ link "Précédent" (Some (page - 1)) ]
  | page when page > min && page < max ->
      [ link "Précédent" (Some (page - 1)); link "Suivant" (Some (page + 1)) ]
  | _ -> []

let private_main ~page ~link ~service feeds count =
  feeds >>= fun feeds ->
  count >>= fun count ->
  User.get_offset () >>= fun off ->
  main_style
    feeds
    (let n = Int64.to_int (Sql.get count#n) in
     let offset = Int32.to_int off in
     link_footer
       ~link
       0
       ((n / offset) - (if (n mod offset) = 0 then 1 else 0))
       page
    )

let private_register () =
  main_style
    [Html.post_form
        ~a:[Html.a_class ["box"]]
        ~service:Services.add_user
        (fun (username_name, (email_name, (password_name, password_check))) -> [
          Html.h1 [Html.pcdata "Inscription"];
          Html.p [
            string_input_box
              ~a:[Html.a_placeholder "Pseudo"]
              ~input_type:`Text
              ~name:username_name
              ();
            Html.br ();
            string_input_box
              ~a:[Html.a_placeholder "Mot de passe"]
              ~input_type:`Password
              ~name:password_name
              ();
            Html.br ();
            string_input_box
              ~a:[Html.a_placeholder "Confirmation"]
              ~input_type:`Password
              ~name:password_check
              ();
            Html.br ();
            string_input_box
              ~a:[Html.a_placeholder "Email"]
              ~input_type:`Text
              ~name:email_name
              ();
            Html.br ();
            submit_input ~value:"Valider" ()
          ]
        ])
        ()
    ]
    []

let feed feeds =
  feeds >>= fun feeds ->
  main_style feeds []

let private_preferences () =
  User.get_user () >>= fun user ->
  main_style
    (match user with
      | None ->
        [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Veuillez vous connecter pour accéder aux préférences."]
        ]
      | Some usr ->
        [ Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.update_user_password
            (fun (password_name, password_check) -> [
              Html.h1 [Html.pcdata "Modifier le mot de passe"] ;
              Html.p [
                string_input_box
                  ~a:[Html.a_placeholder "Nouveau mot de passe"]
                  ~input_type:`Password
                  ~name:password_name
                  ();
                Html.br ();
                string_input_box
                  ~a:[Html.a_placeholder "Confirmer le nouveau mot de passe"]
                  ~input_type:`Password
                  ~name:password_check
                  ();
                Html.br ();
                submit_input ~value:"Valider" ()
              ]
            ])
            ();
          Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.update_user_mail
            (fun email_name -> [
              Html.h1 [Html.pcdata "Changer d'adresse mail"];
              Html.p [
                string_input_box
                  ~a:[Html.a_placeholder User.(usr.email);
                      Html.a_id "new_email"
                     ]
                  ~input_type:`Text
                  ~name:email_name
                  ();
                Html.br ();
                submit_input ~value:"Valider" ()
              ]
            ])
            ();
          Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.update_user_feeds_per_page
            (fun nb_feeds_name -> [
              Html.h1 [Html.pcdata "Changer le nombre de liens par page"];
              Html.p [
                Html.int_input
                  ~a:[Html.a_class ["input-box"];
                      Html.a_placeholder (Int32.to_string
                                                    User.(usr.feeds_per_page))
                     ]
                  ~input_type:`Text
                  ~name:nb_feeds_name
                  ();
                Html.br ();
                submit_input ~value:"Valider" ()
              ]
            ])
            ()
        ]
    )
    []

let private_comment id =
  Db_feed.exist ~feedid:id () >>= fun exist ->
  if not exist then
        main_style [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Ce commentaire n'existe pas."]
        ] []
  else
  User.is_connected () >>= fun state ->
  Feeds.branch_to_html id >>= fun branch ->
    main_style
    ( if not state then
        [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Veuillez vous connecter pour poster un commentaire."]
        ]
      else
        [ branch; Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.append_link_comment
            (fun (parent, (url, (desc, tags))) -> [
              Html.h1 [Html.pcdata "Lien"] ;
              Html.p [
                string_input_box
                  ~a:[Html.a_placeholder "URL"]
                  ~input_type:`Text
                  ~name:url
                  ();
                Html.br ();
                string_input_box
                  ~a:[Html.a_placeholder "Titre"]
                  ~input_type:`Text
                  ~name:desc
                  ();
                Html.br ();
                string_input_box
                  ~a:[Html.a_placeholder "Tags"]
                  ~input_type:`Text
                  ~name:tags
                  ();
                Html.br ();
                Html.int_input
                  ~input_type:`Hidden
                  ~name:parent
                  ~value:(Int32.to_int id)
                  ();
                submit_input ~value:"Envoyer !" ()
              ]
            ])
            ();
          Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.append_desc_comment
            (fun (parent, desc) -> [
              Html.h1 [Html.pcdata "Commentaire"];
              Html.p [
                Html.textarea
                  ~a:[Html.a_class ["input-box"];
                      Html.a_placeholder "Texte"
                     ]
                  ~name:desc
                  ();
               Html.int_input
                  ~input_type:`Hidden
                  ~name:parent
                  ~value:(Int32.to_int id)
                  ();
               Html.br ();
               submit_input ~value:"Envoyer !" ()
              ]
            ])
            ()
        ]) []

let private_edit_feed id =
  Db_feed.exist ~feedid:id () >>= fun exist ->
  if not exist then
        main_style [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Ce commentaire n'existe pas."]
        ] []
  else
  User.is_connected () >>= fun state ->
  Feeds.branch_to_html id >>= fun branch ->
  Feed.get_edit_infos id >>= fun (is_url, edit_desc, edit_url, edit_tags) ->
  User.get_userid () >>= (function
  | None -> Lwt.return true
  | Some uid -> Db_feed.is_feed_author ~feed:id ~userid:uid ())
  >>= fun is_author ->
  main_style
    ( if (not state) or (not is_author) then
        [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Vous n'avez pas le droit d'editer ce lien."]
        ]
      else
        [ branch;
	  if is_url then
            (Html.post_form
               ~a:[Html.a_class ["box"]]
               ~service:Services.edit_link_comment
               (fun (parent, (url, (desc, tags))) -> [
                 Html.h1 [Html.pcdata "Lien"] ;
                 Html.p [
                   string_input_box
                     ~a:[ Html.a_placeholder "URL"]
                     ~input_type:`Text
                     ~name:url
                     ~value:edit_url
                     ();
                   Html.br ();
                   string_input_box
                     ~a:[ Html.a_placeholder "Titre" ]
                     ~input_type:`Text
                     ~name:desc
                     ~value:edit_desc
                     ();
                   Html.br ();
                   string_input_box
                     ~a:[ Html.a_placeholder "Tags" ]
                     ~input_type:`Text
                     ~name:tags
                     ~value:edit_tags
                     ();
                   Html.br ();
                   Html.int_input
                     ~input_type:`Hidden
                     ~name:parent
                     ~value:(Int32.to_int id)
                     ();
                   submit_input ~value:"Envoyer !" ()
                 ]
               ])
               ();)
          else
            (Html.post_form
               ~a:[Html.a_class ["box"]]
               ~service:Services.edit_desc_comment
               (fun (parent, desc) -> [
                 Html.h1 [Html.pcdata "Commentaire"];
                 Html.p [
                   Html.textarea
                     ~a:[Html.a_class ["input-box"];
                         Html.a_placeholder "Comment" ]
                     ~name:desc
                     ~value:edit_desc
                     () ;
                   Html.int_input
                     ~input_type:`Hidden
                     ~name:parent
                     ~value:(Int32.to_int id)
                     ();
                   Html.br ();
                   submit_input ~value:"Envoyer !" ()
                 ]
               ])
               ())
        ]
    )
    []

let feed_list ~service page link feeds nb_feeds =
  User.get_offset () >>= fun off ->
  let starting = Int32.mul (Int32.of_int page) off in
  feeds ~starting ~number:off () >>= fun feedlist ->
  private_main ~page ~link
    ~service
    (Feeds.to_html feedlist)
    nb_feeds

(* see TODO [1] *)
let main ?(page=0) ~service () =
  feed_list ~service page
    (fun name param ->
      Html.a ~service:Services.main [
        Html.pcdata name
      ] param
    )
    (Db_feed.get_root_feeds)
    (Db_feed.count_root_feeds ())

let user ?(page=0) ~service username =
  feed_list ~service page
    (fun name param ->
      Html.a ~service:Services.author_feed [
        Html.pcdata name
      ] (param, username)
    )
    (Db_feed.get_feeds_with_author username)
    (Db_feed.count_feeds_with_author username)

let tag ?(page=0) ~service tag =
  feed_list ~service page
    (fun name param ->
      Html.a ~service:Services.tag_feed [
        Html.pcdata name
      ] (param, tag)
     )
    (Db_feed.get_feeds_with_tag tag)
    (Db_feed.count_feeds_with_tag tag)

let fav_feed ?(page=0) ~service username =
  feed_list ~service page
    (fun name param ->
      Html.a ~service:Services.fav_feed [
        Html.pcdata name
      ] (username, param)
    )
    (Db_feed.get_fav_with_username username)
    (Db_feed.count_fav_with_username username)

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  Db_feed.exist ~feedid:(Int32.of_int id) () >>= fun exist ->
  if exist then
    Feeds.comments_to_html (Int32.of_int id) >>= (fun feed ->
      main_style [feed] [])
  else
    main_style
      [Html.div
          ~a:[Html.a_class ["box"]]
          [Html.pcdata "Ce lien n'existe pas."]
      ]
      []

let register () =
  private_register ()

let preferences () =
  private_preferences ()

let comment id =
  private_comment (Int32.of_int id)

let edit_feed id =
  private_edit_feed (Int32.of_int id)

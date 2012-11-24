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

  let input_value field =
    let input = Eliom_content.Html5.To_dom.of_input field in
    Js.to_string input##value
}}

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
        Eliom_service.onload {{
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
                       ["forms.css"]
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
			  Html.Raw.a ~a: [Html.a_href (Html.uri_of_string (fun ()
			    -> "http://bitbucket.com/Engil/cumulus"))]
			  [Html.pcdata "Cumulus Project"];
			  Html.pcdata ", with love, and the ";
			  Html.Raw.a ~a: [Html.a_href (Html.uri_of_string (fun ()
			    -> "http://ocsigen.org/"))]
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

let link_footer ~link min max page = match page with
  | n when n = min && n < max -> [ link "Suivant" (Some (page + 1)) ]
  | n when n = max && n > min -> [ link "Précédent" (Some (page - 1)) ]
  | n ->
      if n > min && n < max then
        [ link "Précédent" (Some (page - 1)); link "Suivant" (Some (page + 1)) ]
      else []

let reload_feeds service =
  Eliom_service.onload {{
    let service = %service in
    let event = %Feeds.event in
    let stream = Lwt_react.E.to_stream event in
    Lwt.ignore_result
      (Lwt_stream.iter_s
         (fun () ->
           Eliom_client.change_page ~service () ()
         )
         stream
      )
  }}

let private_main ~page ~link ~service feeds count =
  ignore (reload_feeds service);
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
       ((n / offset) - (if n mod offset = 0 then 1 else 0))
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
            Html.string_input
              ~a:[Html.a_class ["input-box"]; Html.a_placeholder "Pseudo"]
              ~input_type:`Text
              ~name:username_name
              ();
            Html.br ();
            Html.string_input
              ~a:[Html.a_class ["input-box"]; Html.a_placeholder "Mot de passe"]
              ~input_type:`Password
              ~name:password_name
              ();
            Html.br ();
            Html.string_input
              ~a:[Html.a_class ["input-box"]; Html.a_placeholder "Confirmation"]
              ~input_type:`Password
              ~name:password_check
              ();
            Html.br ();
            Html.string_input
              ~a:[Html.a_class ["input-box"]; Html.a_placeholder "Email"]
              ~input_type:`Text
              ~name:email_name
              ();
            Html.br ();
            Html.string_input
              ~a:[Html.a_class ["btn-box"]]
              ~input_type:`Submit
              ~value:"Valider"
              ()
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
                Html.string_input
                  ~a:[Html.a_class ["input-box"];
                      Html.a_placeholder "Nouveau mot de passe"
                     ]
                  ~input_type:`Password
                  ~name:password_name
                  ();
                Html.br ();
                Html.string_input
                  ~a:[Html.a_class ["input-box"];
                      Html.a_placeholder "Confirmer le nouveau mot de passe";
                     ]
                  ~input_type:`Password
                  ~name:password_check
                  ();
                Html.br ();
                Html.string_input
                  ~a:[Html.a_class ["btn-box"]]
                  ~input_type:`Submit
                  ~value:"Valider"
                  ()
              ]
            ])
            ();
          Html.post_form
            ~a:[Html.a_class ["box"]]
            ~service:Services.update_user_mail
            (fun email_name -> [
              Html.h1 [Html.pcdata "Changer d'adresse mail"];
              Html.p [
                Html.string_input
                  ~a:[Html.a_class ["input-box"];
                      Html.a_placeholder User.(usr.email);
                      Html.a_id "new_email"
                     ]
                  ~input_type:`Text
                  ~name:email_name
                  ();
                Html.br ();
                Html.string_input
                  ~a:[Html.a_class ["btn-box"]]
                  ~input_type:`Submit
                  ~value:"Valider"
                  ()
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
                Html.string_input
                  ~a:[Html.a_class ["btn-box"]]
                  ~input_type:`Submit
                  ~value:"Valider"
                  ()
              ]
            ])
            ()
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
    (Db.get_feeds)
    (Db.count_feeds ())

let user ?(page=0) ~service username =
  feed_list ~service page
    (fun name param ->
      Html.a ~service:Services.author_feed [
        Html.pcdata name
      ] (param, username)
      )
    (Db.get_feeds_with_author username)
    (Db.count_feeds_with_author username)

let tag ?(page=0) ~service tag =
  feed_list ~service page
    (fun name param ->
      Html.a ~service:Services.tag_feed [
        Html.pcdata name
      ] (param, tag)
     )
    (Db.get_feeds_with_tag tag)
    (Db.count_feeds_with_tag tag)

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  Feeds.feed_id_to_html (Int32.of_int id) >>= fun feed ->
  main_style feed []

let register () =
  private_register ()

let preferences () =
  private_preferences ()

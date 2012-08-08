let main_style data =
  Lwt.return
    (Html.html
       (Html.head
          (Html.title
             (Html.pcdata "Cumulus")
          ) [
            Html.css_link
              ~uri: (Html.make_uri
                       ~service: (Eliom_service.static_dir ())
                       ["style.css"]
              ) ()
          ]
       )
       (Html.body [
         (Html.div
            ~a: [Html.a_class ["container"]]
            data
         )
        ])
    )

let user_form () =
  [
    Html.post_form
      ~a: [Html.a_class ["well form-inline"]]
      ~service: Services.auth
      (fun (user_name, password_name) -> [
        Html.p [
          Html.pcdata "Nickname ";
          Html.string_input
            ~a: [Html.a_class ["input-small"]]
            ~input_type: `Text
            ~name: user_name ();
          Html.pcdata " Password ";
          Html.string_input
            ~a: [Html.a_class ["input-small"]]
            ~input_type: `Password
            ~name: password_name ();
          Html.string_input
            ~a: [Html.a_class ["btn btn-primary"]]
            ~input_type: `Submit
            ~value: "Login" ();
          Html.pcdata " Pas de compte ? ";
          Html.a
            Services.registration
            [Html.pcdata "S'inscrire."] ()
        ]
      ]) ()
  ]

let user_information user =
  [
    Html.div
      ~a: [Html.a_class ["container"]]
      [ Html.pcdata user#!name ]
  ]

let private_main msg feeds =
  feeds >>= fun feeds ->
  User.get_login_state () >>= fun login_state ->
  User.to_html user_information user_form >>= fun user ->
  main_style
    (user @
        [Html.post_form
            ~a: [Html.a_class ["well form-inline"]]
            ~service: Services.append_feed
            (fun (url_name, (title_name, tags_name)) -> [
              Html.p [
                Html.pcdata "URL ";
                Html.string_input
                  ~a: [Html.a_class ["input-medium search-query"]]
                  ~input_type: `Text
                  ~name: url_name ();
                Html.pcdata " Titre du post ";
                Html.string_input
                  ~a: [Html.a_class ["input-medium search-query"]]
                  ~input_type: `Text
                  ~name: title_name ();
                Html.pcdata " Tags ";
                Html.string_input
                  ~a: [Html.a_class ["input-medium search-query"]]
                  ~input_type: `Text
                  ~name: tags_name ();
                Html.string_input
                  ~a: [Html.a_class ["btn btn-primary"]]
                  ~input_type: `Submit
                  ~value: "Send" ()
                  ]
            ]) None
        ] @ msg @ (Utils.msg login_state) @ feeds @ [
          Html.br ();
          Html.footer ~a: [Html.a_class ["footer"]] [
            Html.pcdata "Cumulus project";
          ]
        ]
    )

let private_register () =
  main_style
    [Html.post_form
        ~a: [Html.a_class ["well form-inline"]]
        ~service: Services.add_user
        (fun (username_name, (email_name, (password_name, password_check))) -> [
          Html.p [
            Html.pcdata "Nom d'utilisateur: ";
            Html.string_input
              ~a: [Html.a_class ["input-small"]]
              ~input_type: `Text
              ~name: username_name ();
            Html.br ();
            Html.pcdata "Mot de passe: ";
            Html.string_input
              ~a: [Html.a_class ["input-small"]]
              ~input_type: `Password
              ~name: password_name ();
            Html.br ();
            Html.pcdata "Mot de passe: ";
            Html.string_input
              ~a: [Html.a_class ["input-small"]]
              ~input_type: `Password
              ~name: password_check ();
            Html.br ();
            Html.pcdata "Email: ";
            Html.string_input
              ~a: [Html.a_class ["input-small"]]
              ~input_type: `Text
              ~name: email_name ();
            Html.br ();
            Html.string_input
              ~a: [Html.a_class ["btn btn-primary"]]
              ~input_type: `Submit
              ~value: "Send" ()
          ]
        ]) None
    ]

let feed feeds =
  feeds >>= fun feeds ->
  User.is_connected () >>= fun state ->
  main_style feeds

let private_preferences () =
  User.is_connected () >>= fun state ->
  main_style (
    if not state then
      [Html.pcdata "Veuillez vous connecter pour acceder aux preferences."]
    else
      [Html.post_form
          ~a: [Html.a_class ["well form-inline"]]
          ~service: Services.update_user
          (fun ((email_name, (password_name, password_check))) -> [
            Html.p [
              Html.pcdata "Mot de passe: ";
              Html.string_input
                ~a: [Html.a_class ["input-small"]]
                ~input_type: `Password
                ~name: password_name ();
              Html.br ();
              Html.pcdata "Mot de passe: ";
              Html.string_input
                ~a: [Html.a_class ["input-small"]]
                ~input_type: `Password
                ~name: password_check ();
              Html.br ();
              Html.pcdata "Email: ";
              Html.string_input
                ~a: [Html.a_class ["input-small"]]
                ~input_type: `Text
                ~name: email_name ();
              Html.br ();
              Html.string_input
                ~a: [Html.a_class ["btn btn-primary"]]
                ~input_type: `Submit
                ~value: "Send" ()
            ]
          ]) None
      ]
  )

(* see TODO [1] *)
let main ?(page=0) msg =
  let starting = Int32.of_int (page * 20) in
  private_main msg (Feeds.to_html ~starting:starting ())

let user ?(page=0) msg username =
  let starting = Int32.of_int (page * 20) in
  private_main msg (Feeds.author_to_html ~starting:starting username)

let tag ?(page=0) msg tag =
  let starting = Int32.of_int (page * 20) in
  private_main msg (Feeds.tag_to_html ~starting:starting tag)

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  let id = Int32.of_int id in
  feed (Feeds.feed_id_to_html id)

let register () =
  private_register ()

let preferences msg =
  private_preferences msg

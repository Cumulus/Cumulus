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
                       ["knacss.css"]
              ) ();
            Html.css_link
              ~uri: (Html.make_uri
                       ~service: (Eliom_service.static_dir ())
                       ["forms.css"]
              ) ();
          ]
       )
       (Html.body [
         (Html.div
            ~a: [Html.a_class ["container"]]
            data
         )
       ])
    )

let user_form () = Lwt.return [
  Html.header
    ~a: [Html.a_class ["line";"mod"]]
    [
      Html.div
        ~a: [Html.a_class ["mod left"]] [
          Html.div
            ~a: [Html.a_class ["title"]][
              Html.pcdata "Cumulus Project"]];
      Html.div
        ~a: [Html.a_class ["mod";"right"]][
          Html.post_form
            ~a: [Html.a_class ["right"]]
            ~service: Services.auth
            (fun (user_name, password_name) -> [
              Html.string_input
                ~a: [Html.a_placeholder "Nickname"]
                ~input_type: `Text
                ~name: user_name ();
              Html.string_input
                ~a: [Html.a_placeholder "Password"]
                ~input_type: `Password
                ~name: password_name ();
              Html.string_input
                ~input_type: `Submit
                ~value: "Login" ();
              Html.a
                ~a: [Html.a_class ["nav"]]
                ~service: Services.registration
                [Html.pcdata "Register"] ();
             ]
            ) ()
        ]
    ]
]

let user_information user = Lwt.return [
  Html.header
    ~a: [Html.a_class ["line mod"]]
    [
      Html.div
        ~a: [Html.a_class ["mod left"]] [
          Html.div
            ~a: [Html.a_class ["title"]][
              Html.pcdata "Cumulus Project"]
        ];
      Html.div
        ~a: [Html.a_class ["mod";"right"]][
          Html.post_form
            ~a: [Html.a_class ["right"]]
            ~service: Services.disconnect
            (fun () ->
              [
                Html.p [
                  Html.a ~a: [Html.a_class ["nav"]]
                    ~service: Services.preferences
                    [Html.pcdata "Preferences"] ();
                  Html.string_input
                    ~input_type: `Submit
                    ~value: "Deconnexion"
                    ();
                  Html.img
                    ~alt: (user#!name)
                    ~src: (
                      Html.make_uri
                        ~service: (Utils.get_gravatar (user#!email))
                        (30, "identicon")) ();
                ]])
            ()
        ]
    ]
]

let user_info () =
  User.get_user_and_email () >>= function
    | Some user -> user_information user
    | None -> user_form ()

let private_main ~page ~link msg feeds =
  feeds >>= fun feeds ->
  User.get_login_state () >>= fun login_state ->
  user_info () >>= fun user ->
  main_style
    (user @
       [ Html.div
           ~a: [Html.a_class ["dash"]][
             Html.post_form
               ~service: Services.append_feed
               (fun (url_name, (title_name, tags_name)) -> [
                 Html.string_input
                   ~a: [Html.a_placeholder "URL"]
                   ~input_type: `Text
                   ~name: url_name ();
                 Html.string_input
                   ~a: [Html.a_placeholder "Title"]
                   ~input_type: `Text
                   ~name: title_name ();
                 Html.string_input
                   ~a: [Html.a_placeholder "Tags"]
                   ~input_type: `Text
                   ~name: tags_name ();
                 Html.string_input
                   ~a: [Html.a_class ["btn btn-primary"]]
                   ~input_type: `Submit
                   ~value: "Post it !" ()
               ]) None
           ]] @ msg @ (Utils.msg login_state) @ feeds @ [
         Html.div ~a: [Html.a_class ["footer"]][
           link "Previous" (Some (page - 1));
           link "Next" (Some (page + 1));
           Html.br ();
           Html.br ();
           Html.pcdata "(not so) Proudly propulsed by the inglorious Cumulus Project, love, and the OCaml web Framework Ocsigen"
         ]
       ]
    )

let private_register () =
     User.is_connected () >>= fun state ->
  user_info () >>= fun user ->
  main_style (
    user @
      [Html.post_form
          ~a: [Html.a_class ["box"]]
          ~service: Services.add_user
          (fun (username_name, (email_name, (password_name, password_check))) -> [
            Html.h1 [Html.pcdata "Register"];
            Html.p [
              Html.string_input
                ~a: [Html.a_class ["input-box"]; Html.a_placeholder "Username"]
                ~input_type: `Text
                ~name: username_name ();
              Html.br ();
              Html.string_input
                ~a: [Html.a_class ["input-box"]; Html.a_placeholder "Password"]
                ~input_type: `Password
                ~name: password_name ();
              Html.br ();
              Html.string_input
                ~a: [Html.a_class ["input-box"]; Html.a_placeholder "Confirm password"]
                ~input_type: `Password
                ~name: password_check ();
              Html.br ();
              Html.string_input
                ~a: [Html.a_class ["input-box"]; Html.a_placeholder "Email"]
                ~input_type: `Text
                ~name: email_name ();
              Html.br ();
              Html.string_input
                ~a: [Html.a_class ["btn-box"]]
                ~input_type: `Submit
                ~value: "Send" ()
            ]
          ]) None
      ]
  )

let feed feeds =
  feeds >>= fun feeds ->
  User.is_connected () >>= fun state ->
  main_style feeds

let private_preferences msg =
  User.is_connected () >>= fun state ->
  user_info () >>= fun user ->
  main_style (
    user @ msg @
      if not state then
        [Html.pcdata "Veuillez vous connecter pour acceder aux preferences."]
      else
        [
          Html.post_form
            ~a: [Html.a_class ["box"]]
            ~service: Services.update_user_password
            (fun ((password_name, password_check)) -> [
              Html.h1 [Html.pcdata "Change password"] ;
              Html.p [
                Html.string_input
                  ~a: [Html.a_class ["input-box"]; Html.a_placeholder "New password"]
                  ~input_type: `Password
                  ~name: password_name ();
                Html.br ();
                Html.string_input
                  ~a: [Html.a_class ["input-box"]; Html.a_placeholder "Confirm new password"]
                  ~input_type: `Password
                  ~name: password_check ();
                Html.br ();
                Html.string_input
                  ~a: [Html.a_class ["btn-box"]]
                  ~input_type: `Submit
                  ~value: "Send" ()
              ]
            ]) ()
        ]
        @
          [Html.post_form
              ~a: [Html.a_class ["box"]]
              ~service: Services.update_user_mail
              (fun ((email)) -> [
                Html.h1 [Html.pcdata "Change mail"];
                Html.p [
                  Html.string_input
                    ~a: [Html.a_class ["input-box"]; Html.a_placeholder "New Mail"]
                    ~input_type: `Text
                    ~name: email ();
                  Html.br ();
                  Html.string_input
                    ~a: [Html.a_class ["btn-box"]]
                    ~input_type: `Submit
                    ~value: "Send" ()
                ]
              ]) ()
          ]
  )

(* see TODO [1] *)
let main ?(page=0) msg =
  let starting = Int32.of_int (page * 20) in
  private_main ~page
    ~link:(fun name param ->
      Html.a ~service:Services.main [
        Html.pcdata name
      ] param
    ) msg (Feeds.to_html ~starting:starting ())

let user ?(page=0) msg username =
  let starting = Int32.of_int (page * 20) in
  private_main ~page
    ~link:(fun name param ->
      Html.a ~service:Services.author_feed [
        Html.pcdata name
      ] (param, username)
    ) msg (Feeds.author_to_html ~starting:starting username)

let tag ?(page=0) msg tag =
  let starting = Int32.of_int (page * 20) in
  private_main ~page
    ~link:(fun name param ->
      Html.a ~service:Services.tag_feed [
        Html.pcdata name
      ] (param, tag)
    ) msg (Feeds.tag_to_html ~starting:starting tag)

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  let id = Int32.of_int id in
  feed (Feeds.feed_id_to_html id)

let register () =
  private_register ()

let preferences msg =
  private_preferences msg


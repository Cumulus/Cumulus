let private_main msg feeds =
  feeds >>= (fun feeds ->
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
            (Html.div ~a: [Html.a_class ["container"]]
              (msg @ feeds @ [
                Html.post_form
                  ~a: [Html.a_class ["well form-inline"]]
                  ~service: Services.append_feed
                  (fun (url_name, (title_name, tags_name)) -> [
                    Html.p [
                      Html.pcdata  "URL ";
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
                  ]) ();
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
                      Html.pcdata "Pas de compte ? ";
                      Html.a
                        Services.registration
                        [Html.pcdata "S'inscrire."] ()
                    ]
                  ]) ();
                Html.br ();
                Html.footer ~a: [Html.a_class ["footer"]] [
                Html.pcdata "Cumulus project";
                ]
              ])
            )
         ])
      )
  )

let main msg =
  private_main msg (Feeds.to_html ())

let user msg username =
  private_main msg (Feeds.author_to_html username)

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
            (Html.div ~a: [Html.a_class ["content"]]
              (msg @ feeds @ [
                Html.get_form Services.append_feed
                  (fun (url_name, (title_name, tags_name)) -> [
                    Html.p [
                      Html.string_input
                        ~input_type: `Text
                        ~name: url_name ();
                      Html.string_input
                        ~input_type: `Text
                        ~name: title_name ();
                      Html.string_input
                        ~input_type: `Text
                        ~name: tags_name ();
                      Html.string_input
                        ~input_type: `Submit
                        ~value: "Send" ()
                    ]
                  ]);
                Html.post_form Services.auth
                  (fun (user_name, password_name) -> [
                    Html.p [
                      Html.string_input
                        ~input_type: `Text
                        ~name: user_name ();
                      Html.string_input
                        ~input_type: `Text
                        ~name: password_name ();
                      Html.string_input
                        ~input_type: `Submit
                        ~value: "Login" ()
                    ]
                  ]) ();
                Html.br ();
                Html.a Services.registration [Html.pcdata "registration"] ()
              ])
            )
         ])
      )
  )

let main msg =
  private_main msg (Feeds.to_html ())

let user msg username =
  private_main msg (Feeds.author_to_html username)

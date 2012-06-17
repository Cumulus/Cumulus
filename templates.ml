let main feeds l link_service auth_service registration_service =
  Feeds.to_html feeds >>= (fun feeds ->
    l >>= (fun l ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Cumulus")) [])
           (Html.body
              (l @ feeds @ [
                Html.get_form link_service
                  (fun (url_name, title_name) -> [
                    Html.p [
                      Html.string_input
                        ~input_type: `Text
                        ~name: url_name ();
                      Html.string_input
                        ~input_type: `Text
                        ~name: title_name ();
                      Html.string_input
                        ~input_type: `Submit
                        ~value: "Send" ()
                    ]
                  ]);
                Html.post_form auth_service
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
                Html.a registration_service [Html.pcdata "registration"] ()
              ])
           )
        )
    )
  )

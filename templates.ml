let private_main l f =
  f () >>= (fun feeds ->
    Lwt.return
      (Html.html
         (Html.head (Html.title (Html.pcdata "Cumulus")) [])
         (Html.body
            (l @ feeds @ [
              Html.get_form Services.append_feed
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
      )
  )

let main l =
  private_main l Feeds.to_html

let user l username =
  private_main l (fun () -> Feeds.author_to_html username)

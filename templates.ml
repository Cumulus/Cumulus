let main feeds l service =
  Feeds.to_html feeds >>= (fun feeds ->
    l >>= (fun l ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Cumulus")) [])
           (Html.body
              (l @ feeds @ [
                Html.get_form service
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
                  ])
              ])
           )
        )
    )
  )

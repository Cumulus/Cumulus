module Html = Eliom_pervasives.HTML5

let main_service =
  Eliom_output.Html5.register_service
    ~path: [""]
    ~get_params: Eliom_parameters.unit
    (fun () () ->
      Lwt.return
        (Html.html
           (Html.head (Html.title (Html.pcdata "Hello World")) [])
           (Html.body [
             Html.p [
               Html.pcdata "Hello World"
             ]
           ])
        )
    )

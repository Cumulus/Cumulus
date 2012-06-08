open Eliom_pervasives.HTML5

let main_service =
  Eliom_output.Html5.register_service
  ~path: [""]
  ~get_params: Eliom_parameters.unit
  (fun () () ->
    Lwt.return
    (html
      (head (title (pcdata "Hello World")) [])
      (body [
        p [
          pcdata "Hello World"
        ]
      ])
    )
  )

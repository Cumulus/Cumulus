let atom =
  Eliom_service.service
    ~path: ["atom.xml"]
    ~get_params: Eliom_parameter.unit
    ()

let main =
  Eliom_service.service
    ~path: [""]
    ~get_params: Eliom_parameter.unit
    ()

let append_feed =
  Eliom_service.post_service
    ~fallback: main
    ~post_params: Eliom_parameter.((string "url") **
                                      (string "title") **
                                      (string "tags"))
    ()

let author_feed =
  Eliom_service.service
    ~path: [""]
    ~get_params: (Eliom_parameter.string "username")
    ()

let auth =
  (* post_coservice' *)
  Eliom_service.post_service
    ~fallback: main
    ~post_params: Eliom_parameter.((string "username") ** (string "password"))
    ()

let add_user =
  Eliom_service.post_service
    ~fallback: main
    ~post_params: Eliom_parameter.((string "username") **
                                      (string "password") **
                                      (string "email"))
    ()

let registration =
  Eliom_service.service
    ~path: ["registration"]
    ~get_params: Eliom_parameter.unit
    ()

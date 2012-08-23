let atom =
  Eliom_service.service
    ~path: ["atom.xml"]
    ~get_params: Eliom_parameter.unit
    ()

let main =
  Eliom_service.service
    ~path: [""]
    ~get_params: (Eliom_parameter.opt (Eliom_parameter.int "page"))
    ()

let view_feed =
  Eliom_service.service
    ~path:["view"]
    ~get_params: Eliom_parameter.(suffix (int "id" ** string "name"))
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
    ~get_params: Eliom_parameter.(opt (int "page") ** string "username")
    ()

let tag_feed =
  Eliom_service.service
    ~path: [""]
    ~get_params: Eliom_parameter.(opt (int "page") ** string "tag")
    ()

let auth =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((string "username") ** (string "password"))
    ()

let add_user =
  Eliom_service.post_service
    ~fallback: main
    ~post_params: Eliom_parameter.((string "username") **
                                      (string "password") **
                                      (string "password_check") **
                                      (string "email"))
    ()

let update_user =
  Eliom_service.post_service
    ~fallback: main
    ~post_params: Eliom_parameter.((string "password") **
                                      (string "password_check"))
    ()

let registration =
  Eliom_service.service
    ~path: ["registration"]
    ~get_params: Eliom_parameter.unit
    ()

let disconnect =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.unit
    ()

let preferences =
  Eliom_service.service
    ~path: ["preferences"]
    ~get_params: Eliom_parameter.unit
   ()

(*
Copyright (c) 2012 Enguerrand Decorne

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

let atom =
  Eliom_service.service
    ~path: ["cumulus.atom"]
    ~get_params: Eliom_parameter.unit
    ()

let atom_feed =
  Eliom_service.service
    ~path: ["atom"]
    ~get_params: Eliom_parameter.(suffix (int "feed_id"))
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
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((string "url") **
                                      (string "desc") **
                                      (string "tags"))
    ()

let author_feed =
  Eliom_service.service
    ~path: [""]
    ~get_params: Eliom_parameter.(opt (int "page") ** string "username")
    ()

let fav_feed =
  Eliom_service.service
    ~path:["fav"]
    (* ~get_params: Eliom_parameter.(suffix (string "username") ** int "page") *)
    ~get_params: Eliom_parameter.(suffix_prod (string "name") (opt (int "page")))
    ()

let add_fav_feed =
  Eliom_service.coservice'
    ~get_params: (Eliom_parameter.int32 "feed_id")
    ()

let del_fav_feed =
  Eliom_service.coservice'
    ~get_params: (Eliom_parameter.int32 "feed_id")
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
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((string "username") **
                                      (string "password") **
                                      (string "password_check") **
                                      (string "email"))
    ()

let append_link_comment =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((int "id") **
                                   (string "url") **
                                   (string "desc") **
                                   (string "tags"))
    ()

let append_desc_comment =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((int "id") **
                                   (string "desc"))
    ()

let update_user_mail =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((string "email"))
    ()

let update_user_password =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((string "password") **
                                      (string "password_check"))
    ()

let update_user_feeds_per_page =
  Eliom_service.post_coservice'
    ~post_params: Eliom_parameter.((int "feeds_per_page"))
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

let comment =
  Eliom_service.service
    ~path: ["comment"]
    ~get_params: Eliom_parameter.(suffix ((int "id") ** string "name"))
    ()

let delete_feed =
  Eliom_service.coservice'
    ~get_params:(Eliom_parameter.int32 "id")
    ()

let edit_feed =
  Eliom_service.service
    ~path: ["edit"]
    ~get_params: Eliom_parameter.(suffix ((int "id") ** string "name"))
    ()

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

open Batteries
open Eliom_lib.Lwt_ops

let atom =
  Eliom_service.Http.service
    ~path: ["cumulus.atom"]
    ~get_params: Eliom_parameter.unit
    ()

let comments_atom =
  Eliom_service.Http.service
    ~path: ["cumulus-comments.atom"]
    ~get_params: Eliom_parameter.unit
    ()

let atom_feed =
  Eliom_service.Http.service
    ~path: ["atom"; "feed"]
    ~get_params: Eliom_parameter.(suffix (int32 "feed_id"))
    ()

let atom_tag =
  Eliom_service.Http.service
    ~path: ["atom"; "tag"]
    ~get_params: Eliom_parameter.(suffix (string "tag"))
    ()

let main =
  Eliom_service.App.service
    ~path: [""]
    ~get_params: Eliom_parameter.unit
    ()

let view_feed' =
  Eliom_service.App.service
    ~path:["view"]
    ~get_params: Eliom_parameter.(suffix_prod (int32 "id" ** string "name") (opt any))
    ()

let view_feed =
  Eliom_service.App.service
    ~path:["view"]
    ~get_params: Eliom_parameter.(suffix (int32 "id" ** string "name"))
    ()

let append_feed =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.((string "url") **
                                     (string "desc") **
                                     (string "tags"))
    ()

let author_feed =
  Eliom_service.App.service
    ~path: [""]
    ~get_params: Eliom_parameter.(string "username")
    ()

let fav_feed =
  Eliom_service.App.service
    ~path:["fav"]
    ~get_params: Eliom_parameter.(suffix (string "name"))
    ()

let add_fav_feed =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Ok | `NotConnected] Eliom_service.rt)
    ~get_params: (Eliom_parameter.int32 "feed_id")
    ()

let del_fav_feed =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Ok | `NotConnected] Eliom_service.rt)
    ~get_params: (Eliom_parameter.int32 "feed_id")
    ()

let upvote_feed =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Ok of (int * int) | `NotConnected | `NoRight] Eliom_service.rt)
    ~get_params: (Eliom_parameter.int32 "feed_id")
    ()

let downvote_feed =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Ok of (int * int) | `NotConnected | `NoRight] Eliom_service.rt)
    ~get_params: (Eliom_parameter.int32 "feed_id")
    ()

let cancelvote_feed =
  Eliom_service.Ocaml.coservice'
    ~rt:(Eliom_service.rt : [`Ok of (int * int) | `NotConnected | `NoRight] Eliom_service.rt)
    ~get_params: (Eliom_parameter.int32 "feed_id")
    ()

let tag_feed =
  Eliom_service.App.service
    ~path: [""]
    ~get_params: Eliom_parameter.(string "tag")
    ()

let auth =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.((string "username") ** (string "password"))
    ()

let add_user =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.((string "username") **
                                     (string "password") **
                                     (string "password_check") **
                                     (string "email"))
    ()

let append_link_comment =
  Eliom_service.Http.post_coservice
    ~fallback:view_feed
    ~post_params: Eliom_parameter.((int32 "id") **
                                     (string "url") **
                                     (string "desc") **
                                     (string "tags"))
    ()

let append_desc_comment =
  Eliom_service.Http.post_coservice
    ~fallback:view_feed
    ~post_params: Eliom_parameter.((int32 "id") **
                                     (string "desc"))
    ()

let update_user_mail =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.((string "email"))
    ()

let update_user_password =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.((string "password") **
                                     (string "password_check"))
    ()

let update_user_feeds_per_page =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.((int32 "feeds_per_page"))
    ()

let registration =
  Eliom_service.App.service
    ~path: ["registration"]
    ~get_params: Eliom_parameter.unit
    ()

let disconnect =
  Eliom_service.Http.post_coservice'
    ~post_params: Eliom_parameter.unit
    ()

let preferences =
  Eliom_service.App.service
    ~path: ["preferences"]
    ~get_params: Eliom_parameter.unit
    ()

let comment =
  Eliom_service.App.service
    ~path: ["comment"]
    ~get_params: Eliom_parameter.(suffix ((int32 "id") ** string "name"))
    ()

let delete_feed =
  Eliom_service.Http.coservice'
    ~get_params:(Eliom_parameter.int32 "id")
    ()

let edit_feed =
  Eliom_service.App.service
    ~path: ["edit"]
    ~get_params: Eliom_parameter.(suffix ((int32 "id") ** string "name"))
    ()

let edit_link_comment =
  Eliom_service.Http.post_coservice
    ~fallback:view_feed
    ~post_params: Eliom_parameter.((int32 "id") **
                                     (string "url") **
                                     (string "desc") **
                                     (string "tags"))
    ()

let edit_desc_comment =
  Eliom_service.Http.post_coservice
    ~fallback:view_feed
    ~post_params: Eliom_parameter.((int32 "id") **
                                     (string "desc"))
    ()

let reset_password =
  Eliom_service.Http.post_coservice'
    ~post_params:Eliom_parameter.(string "email")
    ()

let reset_password_form =
  Eliom_service.App.service
    ~path:["pwd-reset"]
    ~get_params:Eliom_parameter.unit
    ()

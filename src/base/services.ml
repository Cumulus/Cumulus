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
open Eliom_service
open Eliom_parameter

let atom =
  Http.service
    ~path:["cumulus.atom"]
    ~get_params:unit
    ()

let comments_atom =
  Http.service
    ~path:["cumulus-comments.atom"]
    ~get_params:unit
    ()

let atom_feed =
  Http.service
    ~path:["atom"; "feed"]
    ~get_params:(suffix (int32 "feed_id"))
    ()

let atom_tag =
  Http.service
    ~path:["atom"; "tag"]
    ~get_params:(suffix (string "tag"))
    ()

let main =
  App.service
    ~path:[]
    ~get_params:unit
    ()

let view_feed =
  App.service
    ~path:["view"]
    ~get_params:(suffix (int32 "id" ** string "name"))
    ()

let view_feed' =
  App.service
    ~path:["view"]
    ~get_params:(suffix (int32 "id"))
    ()

let view_feed'' =
  App.service
    ~path:["view"]
    ~get_params:(suffix_prod (int32 "id" ** string "name") any)
    ()

let append_feed =
  Http.post_coservice'
    ~post_params:(string "url" ** string "desc" ** string "tags")
    ()

let author_feed =
  App.service
    ~path:[]
    ~get_params:(string "username")
    ()

let fav_feed =
  App.service
    ~path:["fav"]
    ~get_params:(suffix (string "name"))
    ()

let add_fav_feed =
  Ocaml.post_coservice'
    ~rt:(rt : [`Ok | `NotConnected] rt)
    ~post_params:(int32 "feed_id")
    ()

let del_fav_feed =
  Ocaml.post_coservice'
    ~rt:(rt : [`Ok | `NotConnected] rt)
    ~post_params:(int32 "feed_id")
    ()

let upvote_feed =
  Ocaml.post_coservice'
    ~rt:(rt : [`Ok of (int * int) | `NotConnected | `NoRight] rt)
    ~post_params:(int32 "feed_id")
    ()

let downvote_feed =
  Ocaml.post_coservice'
    ~rt:(rt : [`Ok of (int * int) | `NotConnected | `NoRight] rt)
    ~post_params:(int32 "feed_id")
    ()

let cancelvote_feed =
  Ocaml.post_coservice'
    ~rt:(rt : [`Ok of (int * int) | `NotConnected | `NoRight] rt)
    ~post_params:(int32 "feed_id")
    ()

let tag_feed =
  App.service
    ~path:[]
    ~get_params:(string "tag")
    ()

let auth =
  Http.post_coservice'
    ~post_params:(string "username" ** string "password")
    ()

let add_user =
  Http.post_coservice'
    ~post_params:(string "username" **
                  string "password" **
                  string "password_check" **
                  string "email")
    ()

let append_link_comment =
  Http.post_coservice
    ~fallback:view_feed'
    ~post_params:(int32 "id" **
                  string "url" **
                  string "desc" **
                  string "tags")
    ()

let append_desc_comment =
  Http.post_coservice
    ~fallback:view_feed'
    ~post_params:(int32 "id" ** string "desc")
    ()

let update_user_mail =
  Http.post_coservice'
    ~post_params:(string "email")
    ()

let update_user_password =
  Http.post_coservice'
    ~post_params:(string "password" ** string "password_check")
    ()

let update_user_feeds_per_page =
  Http.post_coservice'
    ~post_params:(int32 "feeds_per_page")
    ()

let registration =
  App.service
    ~path:["registration"]
    ~get_params:unit
    ()

let disconnect =
  Http.post_coservice'
    ~post_params:unit
    ()

let preferences =
  App.service
    ~path:["preferences"]
    ~get_params:unit
    ()

let comment =
  App.service
    ~path:["comment"]
    ~get_params:(suffix (int32 "id" ** string "name"))
    ()

let delete_feed =
  Http.coservice'
    ~get_params:(int32 "id")
    ()

let edit_feed =
  App.service
    ~path:["edit"]
    ~get_params:(suffix (int32 "id" ** string "name"))
    ()

let edit_link_comment =
  Http.post_coservice
    ~fallback:view_feed'
    ~post_params:(int32 "id" ** string "url" ** string "desc" ** string "tags")
    ()

let edit_desc_comment =
  Http.post_coservice
    ~fallback:view_feed'
    ~post_params:(int32 "id" ** string "desc")
    ()

let reset_password =
  Http.post_coservice'
    ~post_params:(string "email")
    ()

let reset_password_form =
  App.service
    ~path:["pwd-reset"]
    ~get_params:unit
    ()

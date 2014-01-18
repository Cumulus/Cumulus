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

module Calendar = CalendarLib.Calendar
module Uri = Eliom_uri

type feed = Db_feed.feed =
  { author : int32
  ; id : int32
  ; date : CalendarLib.Calendar.t
  ; description : string
  ; url : string option
  ; parent: int32 option
  ; root : int32 option
  ; tags : string list
  ; score : int
  ; user : < email_digest : string; name : string >
  ; fav : bool
  ; vote : int
  ; count : int
  }

let links_of_tags tags =
  List.fold_left (fun acc tag ->
    let link =
      Html.a
        ~a:[Html.a_class ["tags"]]
        ~service:Services.tag_feed
        [Html.pcdata tag]
        (None, tag)
    in
    acc @ [Html.pcdata " "; link]
  ) [] tags

module H = Xhtml_f.Make(Eliom_content.Xml)
module M = MarkdownHTML.Make_xhtml(H)

let conv : 'a H.elt list -> 'a Html.elt list = fun x -> Html.totl (H.toeltl x)

let to_html self =
  let get_image cls imgname =
   Html.img ~a: [Html.a_class cls]
                  ~alt: imgname
                  ~src:(Html.make_uri
                    ~service: (Eliom_service.static_dir ())
                  [imgname]
                    )() in
  let content = match self.url with
    | Some url -> Html.div ~a:[Html.a_class["line_title"]][
                    Html.Raw.a
                    ~a:[Html.a_class ["postitle"];
                        Html.a_href (Html.uri_of_string (fun () -> url));
                       ]
                    [Html.pcdata self.description]]
    | None ->
      let markdown = Markdown.parse_text self.description in
      let render_pre ~kind s = H.pre [H.pcdata s] in
      let render_link {Markdown.href_target; href_desc} =
        H.a ~a:[H.a_href (H.uri_of_string href_target)] [H.pcdata href_desc]
      in
      let render_img {Markdown.img_src; img_alt} =
        H.img ~src:(H.uri_of_string img_src) ~alt:img_alt ()
      in
      Html.div ~a:[Html.a_class ["lamalama"]] (conv (M.to_html ~render_pre ~render_link ~render_img markdown))
  in
  let tags = match self.url with
    | Some _ -> Html.div ~a:[Html.a_class["tag_line"]] (links_of_tags self.tags)
    | None -> Html.div ~a:[Html.a_class["error"]][] in
  User.is_connected () >>= fun state ->
  User.get_userid () >>= (function
    | None -> Lwt.return false
    | Some userid -> Lwt.return (Int32.equal userid self.author)
  )
  >>= fun is_author ->
  User.is_admin ()
  >>= fun is_admin ->
  Lwt.return (
    List.flatten
      [
        [
          Html.aside ~a: [Html.a_class ["row";"post";"mod"]; Html.a_id "post"] [
              Html.aside ~a: [Html.a_class["col";"avatarbox"]]
                [Html.div ~a: [Html.a_class["post_avatar"]]
                   [Html.img
                      ~a: [Html.a_class ["postimg"]]
                      ~alt: (self.user#name)
                      ~src: (
                        Html.make_uri
                          ~service: (Utils.get_gravatar (self.user#email_digest)) (65, "identicon")
                      )
                      ()]];
              Html.aside ~a: [Html.a_class["col";"post_info"]][
                Html.div ~a: [Html.a_class["line_author"]]([

                  Html.pcdata ("Publié le " ^ (Utils.string_of_calendar self.date) ^ " par ");
                  Html.a
                    ~service:Services.author_feed
                    [Html.pcdata self.user#name]
                    (None, self.user#name);
                  Html.a
                    ~service:Services.atom_feed
                    [Html.pcdata "  Flux Atom du lien "]
                    (Int32.to_int self.id);
                ]
                @
          (if is_author || is_admin then
            [
                Html.a ~service:Services.delete_feed [Html.pcdata "- Supprimer "] self.id ;
                Html.a ~service:Services.edit_feed [Html.pcdata "- Editer"]
                  (Int32.to_int self.id, Utils.troncate self.description);
            ]
           else []
                ));
                content;
                tags;
              ];
              Html.div ~a: [Html.a_class["col";"post_int"]][


                Html.aside
                  ~a: [Html.a_class["comment_block"]][
                  Html.div ~a: [Html.a_class["com_wrap"]][
                    Html.a
                      ~service:Services.view_feed
                      [if self.count <= 0 then
                         get_image ["circled";"gray";"comment_icon"] "comments.png"
                       else
                         get_image ["circled";"highlighted";"comment_icon"] "comments.png";
                      ]
                      (Int32.to_int self.id, Utils.troncate self.description)];
                  Html.pcdata (string_of_int self.count)
                ];
                Html.div ~a: [Html.a_class ["fav_wrap"]][
                  if self.fav = false then
                    Html.a
                      ~service:Services.add_fav_feed
                      [get_image ["circled";"gray";] "fav.png"]
                      (self.id)
                  else
                    Html.a
                      ~service:Services.del_fav_feed
                      [get_image ["circled";"highlighted";"deletable"] "fav.png"]
                      (self.id);
                ];
                let cl = if self.score <= 0 then ["upvote_wrap_inner";"gray"] else
                    ["upvote_wrap_inner"] in
                Html.div ~a: [Html.a_class["upvote_wrap"]][
                  Html.div ~a: [Html.a_class cl][
                    if self.score <> 1 then
                      (Html.a ~service:Services.upvote_feed [
                         get_image [] "up.png"] self.id)
                    else
                      (Html.a ~service:Services.cancelvote_feed [
                         get_image [] "upon.png"] self.id);
                    Html.pcdata (string_of_int self.score);
                    if self.score <> -1 then
                      (Html.a ~service:Services.downvote_feed [
                         get_image [] "down.png"] self.id)
                    else
                      (Html.a ~service:Services.cancelvote_feed [
                         get_image [] "downon.png"] self.id)
                  ]];
              ]
          ]
        ]
      ]
  )

let to_atom self =
  User.get_userid () >>= fun user ->
  Db_feed.get_root ~feedid:self.id ~user () >>= fun root_feed ->
  let title, root_infos = match root_feed with
    | Some root_feed' -> ("[RE: " ^ (Utils.troncate root_feed'.description) ^
                            "] " ^ (
                            match self.url with
                            | Some url -> self.description
                            | None -> Utils.troncate self.description
                          ),
                          [Html.pcdata "ce message est une réponse à : ";
                           Html.a ~service:Services.view_feed
                             [Html.pcdata root_feed'.description]
                             (Int32.to_int root_feed'.id,
                              Utils.troncate root_feed'.description)])
    | None -> (Utils.troncate' 200 self.description, [])
  in
  Lwt.return (
    Atom_feed.entry
      ~updated: self.date
      ~id:(Int32.to_string self.id)
      ~title: (Atom_feed.plain (title))
      [Atom_feed.authors [Atom_feed.author self.user#name];
       Atom_feed.links [Atom_feed.link (Uri.make_string_uri ~absolute:true
                                          ~service:Services.view_feed
                                          (Int32.to_int self.id, "")
                                       )];
       Atom_feed.summary (Atom_feed.html5 (
         (match self.url with
          | Some url -> Html.Raw.a ~a:
                          [Html.a_href
                             (Html.uri_of_string
                                (fun () -> url)
                             )
                          ]
                          [Html.pcdata self.description]
          | None ->
              let markdown = Markdown.parse_text self.description in
              let render_pre ~kind s = H.pre [H.pcdata s] in
              let render_link {Markdown.href_target; href_desc} =
                H.a ~a:[H.a_href (H.uri_of_string href_target)] [H.pcdata href_desc]
              in
              let render_img {Markdown.img_src; img_alt} =
                H.img ~src:(H.uri_of_string img_src) ~alt:img_alt ()
              in
              Html.div ~a:[Html.a_class ["lamalama"]] (conv (M.to_html ~render_pre
                                                               ~render_link ~render_img markdown))
         )
         :: (Html.br ())
         :: (Html.a ~service:Services.atom_feed [Html.pcdata "Flux atom du lien"]
               (Int32.to_int self.id))
         :: (Html.br ())
         :: (Html.pcdata "Tags : ")
         :: (links_of_tags self.tags)
         @ [(Html.br ())]
         @ root_infos
       )
       )
      ]
  )

let get_edit_url feeds =
  match feeds.url with
  | Some f -> f
  | None -> "Url"

let get_edit_tags = String.concat ", "

let get_edit_infos id =
  User.get_userid () >>= fun user ->
  Db_feed.get_feed_with_id ~user id >>= fun feeds ->
  let desc = feeds.description in
  let url = get_edit_url feeds in
  let tags_str = get_edit_tags feeds.tags in
  Lwt.return (Option.is_some feeds.url, desc, url, tags_str)

let delete_feed_check ~feedid ~userid () =
  User.is_admin () >>= fun is_admin ->
  Db_feed.is_feed_author ~feedid ~userid () >>= fun is_author ->
  if is_admin || is_author then
    Db_feed.delete_feed ~feedid ()
  else
    Lwt.return ()

let exec_if_not_author f feedid =
  User.get_userid () >>= function
  | Some userid ->
      Db_feed.is_feed_author ~feedid ~userid ()
      >>= fun is_author ->
      if not is_author then
        f ~feedid ~userid ()
      else
        Lwt.return ()
  | None -> Lwt.return ()

let get_userid f =
  User.get_userid () >>= BatOption.map_default f Lwt.return_unit

let add_fav feedid =
  get_userid (fun userid -> Db_feed.add_fav ~feedid ~userid ())
let del_fav feedid =
  get_userid (fun userid -> Db_feed.del_fav ~feedid ~userid ())

let upvote = exec_if_not_author Db_feed.upvote
let downvote = exec_if_not_author Db_feed.downvote
let cancel_vote = exec_if_not_author Db_feed.cancelvote

(* TODO: Remove the following functions *)
let get_root_feeds = Db_feed.get_root_feeds
let get_feeds_with_author = Db_feed.get_feeds_with_author
let get_feeds_with_tag = Db_feed.get_feeds_with_tag
let get_fav_with_username = Db_feed.get_fav_with_username
let exist = Db_feed.exist
let is_feed_author = Db_feed.is_feed_author

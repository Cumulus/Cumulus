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

module Html = Eliom_content.Html5.F

(* TODO: improve genericity *)
let feed_list ~service page link feeds =
  User.get_user () >>= fun user ->
  Errors.get_error () >>= fun error ->
  let offset = User.get_feeds_per_page user in
  let starting = Int32.(Int32.of_int page * offset) in
  let userid = Option.map (fun x -> x.User.id) user in
  feeds ~starting ~number:offset ~user:userid () >|= fun (feeds, n) ->
  let feeds = Templates_feeds.to_html ~user feeds in
  let n = Int64.to_int n in
  let offset = Int32.to_int offset in
  Templates_feeds.main_style
    ~user
    ~error
    feeds
    (Templates_feeds.link_footer
       ~link
       0
       ((n / offset) - (if (n mod offset) = 0 then 1 else 0))
       page
    )

(* see TODO [1] *)
let main ?(page=0) ~service () =
  feed_list ~service page
    (fun name param ->
       Html.a ~service:Services.main [
         Html.pcdata name
       ] param
    )
    (Feed.get_root_feeds)

let user ?(page=0) ~service username =
  feed_list ~service page
    (fun name param ->
       Html.a ~service:Services.author_feed [
         Html.pcdata name
       ] (param, username)
    )
    (Feed.get_feeds_with_author username)

let tag ?(page=0) ~service tag =
  feed_list ~service page
    (fun name param ->
       Html.a ~service:Services.tag_feed [
         Html.pcdata name
       ] (param, tag)
    )
    (Feed.get_feeds_with_tag tag)

let fav_feed ?(page=0) ~service username =
  feed_list ~service page
    (fun name param ->
       Html.a ~service:Services.fav_feed [
         Html.pcdata name
       ] (username, param)
    )
    (Feed.get_fav_with_username username)

let feeds_comments_to_html ~user id =
  (* TODO: Avoid duplicate *)
  let userid = Option.map (fun x -> x.User.id) user in
  Feed.get_feed_with_id ~user:userid id >>= fun root ->
  Feed.get_comments ~user:userid id >|= fun comments ->
  let result = Comments.tree_comments [Comments.Sheet root] comments
  in match result with
  | Some tree -> Templates_feeds.comments_to_html' ~user tree
  | None -> Templates_feeds.comments_to_html' ~user (Comments.Sheet root)

(* TODO: Fix it (doesn't seems to work) *)
let feeds_branch_to_html ~user id =
  let userid = Option.map (fun x -> x.User.id) user in
  Feed.get_feed_with_id ~user:userid id >>= fun sheet ->
  match sheet.Feed.root with
  | None ->
      Lwt.return (Templates_feeds.comments_to_html' ~user (Comments.Sheet sheet))
  | Some id ->
      Feed.get_feed_with_id ~user:userid id >>= fun root ->
      Feed.get_comments ~user:userid id >|= fun comments ->
      let tree =
        Comments.branch_comments (Comments.Sheet sheet) (root :: comments)
      in
      Templates_feeds.comments_to_html' ~user tree

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  User.get_user () >>= fun user ->
  Errors.get_error () >>= fun error ->
  Feed.exist ~feedid:(Int32.of_int id) () >>= fun exist ->
  if exist then
    feeds_comments_to_html ~user (Int32.of_int id) >|= fun feed ->
    Templates_feeds.main_style ~user ~error [feed] []
  else
    Lwt.return
      (Templates_feeds.main_style
         ~user
         ~error
         [Html.div
            ~a:[Html.a_class ["box"]]
            [Html.pcdata "Ce lien n'existe pas."]
         ]
         []
      )

let register () =
  User.get_user () >>= fun user ->
  Errors.get_error () >|= fun error ->
  Templates_feeds.private_register ~user ~error ()

let preferences () =
  User.get_user () >>= fun user ->
  Errors.get_error () >|= fun error ->
  Templates_feeds.private_preferences ~user ~error

let comment id =
  let id = Int32.of_int id in
  User.get_user () >>= fun user ->
  Errors.get_error () >>= fun error ->
  Feed.exist ~feedid:id () >|= fun exist ->
  if not exist then
    Templates_feeds.main_style
      ~user
      ~error
      [Html.div
         ~a:[Html.a_class ["box"]]
         [Html.pcdata "Ce commentaire n'existe pas."]
      ] []
  else
    Templates_feeds.private_comment ~user ~error id

let edit_feed id =
  User.get_user () >>= fun user ->
  let userid = Option.map (fun x -> x.User.id) user in
  Feed.get_feed_with_id ~user:userid (Int32.of_int id) >>= fun feed ->
  Errors.get_error () >>= fun error ->
  Feed.get_edit_infos feed.Feed.id >>= fun infos ->
  Feed.exist ~feedid:feed.Feed.id () >|= fun exist ->
  if not exist then
    Templates_feeds.main_style
      ~user
      ~error
      [Html.div
         ~a:[Html.a_class ["box"]]
         [Html.pcdata "Ce commentaire n'existe pas."]
      ] []
  else
    Templates_feeds.private_edit_feed ~user ~error ~feed infos

(* TODO: Put into Templates_common *)
let string_input_box ?(a=[]) =
  Html.string_input ~a:(Html.a_class ["input-box"] :: a)

(* TODO: Put into Templates_common *)
let submit_input ?(a=[]) =
  Html.string_input
    ~a:(Html.a_class ["btn-box"] :: a)
    ~input_type:`Submit

let reset_password () =
  User.get_user () >>= fun user ->
  Errors.get_error () >|= fun error ->
  let form =
    Html.post_form
      ~a:[Html.a_class ["box"]]
      ~service:Services.reset_password
      (fun email_name -> [
           Html.h1 [Html.pcdata "Adresse mail associée au compte"];
           Html.p [
             string_input_box
               ~a:[Html.a_id "new_email"]
               ~input_type:`Text
               ~name:email_name
               ();
             Html.br ();
             submit_input ~value:"Valider" ()
           ]
         ])
      ()
  in
  Templates_feeds.main_style ~user ~error [form] []

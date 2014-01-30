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

let main_style_aux content =
  User.get_user () >>= fun user ->
  Errors.get_error () >>= fun error ->
  content user >|= fun (content, footer) ->
  Templates_feeds.main_style ~user ~error content footer

let feed_list ~service page ~service_link link_param feeds =
  let content user =
    let offset = User.get_feeds_per_page user in
    let starting = Int32.(Int32.of_int page * offset) in
    let userid = User.get_id user in
    feeds ~starting ~number:offset ~user:userid () >|= fun (feeds, n) ->
    let feeds = Templates_feeds.to_html ~user feeds in
    let n = Int64.to_int n in
    let offset = Int32.to_int offset in
    let footer =
      Templates_feeds.link_footer
        ~service:service_link
        ~param:link_param
        0
        ((n / offset) - (if (n mod offset) = 0 then 1 else 0))
        page
    in
    (feeds, footer)
  in
  main_style_aux content

let main_style content =
  let content user =
    content user >|= fun content ->
    (content, [])
  in
  main_style_aux content

let main_style_pure content =
  let content user = Lwt.return (content user) in
  main_style content

(* see TODO [1] *)
let main ?(page=0) ~service () =
  feed_list ~service page
    ~service_link:Services.main
    identity
    Feed.get_root_feeds

let user ?(page=0) ~service username =
  feed_list ~service page
    ~service_link:Services.author_feed
    (fun x -> (x, username))
    (Feed.get_feeds_with_author username)

let tag ?(page=0) ~service tag =
  feed_list ~service page
    ~service_link:Services.tag_feed
    (fun x -> (x, tag))
    (Feed.get_feeds_with_tag tag)

let fav_feed ?(page=0) ~service username =
  feed_list ~service page
    ~service_link:Services.fav_feed
    (fun x -> (username, x))
    (Feed.get_fav_with_username username)

let feeds_comments_to_html ~user id =
  let userid = User.get_id user in
  Feed.get_feed_with_id ~user:userid id >>= fun root ->
  Feed.get_comments ~user:userid id >|= fun comments ->
  let result = Comments.tree_comments [Comments.Sheet root] comments
  in match result with
  | Some tree -> Templates_feeds.comments_to_html' ~user tree
  | None -> Templates_feeds.comments_to_html' ~user (Comments.Sheet root)

(* TODO: Fix it (doesn't seems to work) *)
let feeds_branch_to_html ~user id =
  let userid = User.get_id user in
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
  let content user =
    Feed.exist ~feedid:id () >>= fun exist ->
    if exist then
      feeds_comments_to_html ~user id >|= fun feed ->
      [feed]
    else
      Lwt.return
        (Templates_feeds.error_content "Ce lien n'existe pas.")
  in
  main_style content

let register () =
  let content _ = Templates_feeds.private_register () in
  main_style_pure content

let preferences () =
  let content user = Templates_feeds.private_preferences ~user in
  main_style_pure content

let comment id =
  let content user =
    Feed.exist ~feedid:id () >|= fun exist ->
    if not exist then
      Templates_feeds.error_content "Ce commentaire n'existe pas."
    else
      Templates_feeds.private_comment ~user id
  in
  main_style content

let edit_feed id =
  let content user =
    let userid = User.get_id user in
    Feed.get_feed_with_id ~user:userid id >>= fun feed ->
    Feed.get_edit_infos feed.Feed.id >>= fun infos ->
    Feed.exist ~feedid:feed.Feed.id () >|= fun exist ->
    if not exist then
      Templates_feeds.error_content "Ce commentaire n'existe pas."
    else
      Templates_feeds.private_edit_feed ~user ~feed infos
  in
  main_style content

let reset_password () =
  let form _ = Templates_feeds.reset_password () in
  main_style_pure form

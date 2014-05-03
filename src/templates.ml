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

let main_style_aux page_title content =
  User.get_user () >>= fun user ->
  Errors.get_error () >>= fun error ->
  content user 0 >|= fun (content, server_function) ->
  Templates_feeds.main_style ~user ~error ~server_function ~page_title content

let feed_list feeds =
  let rec content user page =
    let offset = User.get_feeds_per_page user in
    let starting = Int32.(Int32.of_int page * offset) in
    let userid = User.get_id user in
    feeds ~starting ~number:offset ~user:userid () >|= fun feeds ->
    let feeds = Templates_feeds.to_html ~user feeds in
    let server_function ~box =
      let content page = content user page >|= fst in
      Client.feeds_actions ~content ~box
    in
    (feeds, server_function)
  in
  main_style_aux None content

let main_style title content =
  let content user page =
    content user page >|= fun content ->
    (content, fun ~box:_ -> ())
  in
  main_style_aux title content

let main_style_pure content =
  let content user _ = Lwt.return (content user) in
  main_style None content

(* see TODO [1] *)
let main () = feed_list Feed.get_root_feeds

let user username = feed_list (Feed.get_feeds_with_author username)

let tag tag = feed_list (Feed.get_feeds_with_tag tag)

let fav_feed username = feed_list (Feed.get_fav_with_username username)

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
  Feed.get_feed_with_id ~user:userid id >>= fun target ->
  (match target.Feed.root with
    | None -> Lwt.return (Comments.Sheet target)
    | Some root ->
      Feed.get_feed_with_id ~user:userid root >>= fun root ->
      Feed.get_comments ~user:userid root.Feed.id >|= fun comments ->
      (Comments.branch_comments (Comments.Sheet target) (root :: comments)))

(* Shows a specific link (TODO: and its comments) *)
let view_feed id =
  let content user _ =
    Feed.exist ~feedid:id () >>= fun exist ->
    if exist then
      feeds_comments_to_html ~user id >|= fun feeds ->
      [feeds]
    else
      Lwt.return
        (Templates_feeds.error_content "Ce lien n'existe pas.")
  in
  let title = "Feed " ^ (Int32.to_string id)
  in
  main_style (Some title) content

let register () =
  let content _ = Templates_feeds.private_register () in
  main_style_pure content

let preferences () =
  let content user = Templates_feeds.private_preferences ~user in
  main_style_pure content

let comment id =
  let content user _ =
    Feed.exist ~feedid:id () >>= fun exist ->
    if not exist then
      Lwt.return (Templates_feeds.error_content "Ce commentaire n'existe pas.")
    else
      feeds_branch_to_html ~user id >|= fun branch ->
      Templates_feeds.private_comment ~user id branch
  in
  let title = "Comment feed " ^ (Int32.to_string id)
  in
  main_style (Some title) content

let edit_feed id =
  let content user _ =
    let userid = User.get_id user in
    Feed.get_feed_with_id ~user:userid id >>= fun feed ->
    Feed.get_edit_infos feed.Feed.id >>= fun infos ->
    Feed.exist ~feedid:feed.Feed.id () >|= fun exist ->
    if not exist then
      Templates_feeds.error_content "Ce commentaire n'existe pas."
    else
      Templates_feeds.private_edit_feed ~user ~feed infos
  in
  let title = "Edit feed " ^ (Int32.to_string id)
  in
  main_style (Some title) content

let reset_password () =
  let form _ = Templates_feeds.reset_password () in
  main_style_pure form

let atom_aux feeds =
  let user = None in
  let aux self =
    match self.Feed.root with
    | Some root_id ->
        Feed.get_feed_with_id ~user root_id >|= fun root_feed ->
        (Some root_feed, self)
    | None ->
        Lwt.return (None, self)
  in
  Lwt_list.map_p aux feeds

let to_atom () =
  let user = None in
  Feed.get_links_feeds ~user ~starting:0l ~number:Utils.offset ()
  >>= atom_aux
  >|= Templates_atom.to_atom

let comments_to_atom () =
  let user = None in
  Feed.get_comments_feeds ~user ~starting:0l ~number:Utils.offset ()
  >>= atom_aux
  >|= Templates_atom.comments_to_atom

let tag_to_atom tag =
  let user = None in
  Feed.get_feeds_with_tag ~user tag ~starting:0l ~number:Utils.offset ()
  >>= atom_aux
  >|= Templates_atom.tag_to_atom tag

let tree_to_atom id =
  let user = None in
  Feed.get_tree_feeds ~user id ~starting:0l ~number:Utils.offset ()
  >>= atom_aux
  >|= Templates_atom.tree_to_atom id

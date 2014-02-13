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

type feed_generator =
  starting:int32 ->
  number:int32 ->
  user:int32 option ->
  unit ->
  feed list Lwt.t

let get_edit_tags = String.concat ", "

let get_edit_infos id =
  User.get_userid () >>= fun user ->
  Db_feed.get_feed_with_id ~user id >>= fun feed ->
  let tags_str = get_edit_tags feed.tags in
  Lwt.return (feed.description, feed.url, tags_str)

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
      begin Db_feed.is_feed_author ~feedid ~userid () >>= function
      | true -> Lwt.return ()
      | false -> f ~feedid ~userid ()
      end
  | None -> Lwt.return ()

let get_userid f =
  User.get_userid () >>= function
  | Some userid -> f userid >|= fun () -> `Ok
  | None -> Lwt.return `NotConnected

let is_author ~feed user =
  Option.map_default (fun x -> Int32.equal x.User.id feed.author) false user
  || Option.map_default (fun x -> x.User.is_admin) false user

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
let exist = Db_feed.exists
let is_feed_author = Db_feed.is_feed_author
let get_root = Db_feed.get_root
let get_feed_with_id = Db_feed.get_feed_with_id
let get_comments = Db_feed.get_comments
let get_tree_feeds = Db_feed.get_tree_feeds
let get_links_feeds = Db_feed.get_links_feeds
let get_comments_feeds = Db_feed.get_comments_feeds
